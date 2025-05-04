{-# LANGUAGE NamedFieldPuns #-}

module SafeProc where

import Control.Monad (forM, forM_, unless, when)
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Data.Set as S
import qualified UncheckedInstsExt as UIE
import UncheckedProc (UncheckedProc (..))
import qualified UncheckedProc as UP
import ProgWriter
import Data.List (intersperse, intercalate)
import Data.Maybe (isJust, isNothing)

-- The version of UncheckedProc but with:
-- - Named variables only
-- - Explicit function's signatures, Named args
-- - Return with arguments
-- - Call with arguments
-- - Named labels
-- - Named functions
--
-- New variables are introduced in the place of first usage
-- Entry function is still the first one
--
-- TODO: Check that goto/branch instruction is always the last in the block
-- and every blocks ends with goto/branch. It can be done in UncheckedProc

frameOffset :: Int
frameOffset = UIE.varToIdx $ UP.convertVar (UP.Var 0)

newtype Var = Var String

newtype Lbl = Lbl String

newtype Func = Func String

data SafeProc
  = SProcGoto Lbl
  | SProcBranch Var Lbl Lbl
  | SProcConst Var Int
  | SProcCopyAdd Var [Var]
  | SProcCopySub Var [Var]
  | SProcRead Var
  | SProcWrite Var
  | SProcCall Func [Var] [Var]
  | SProcReturn [Var]

data Block = Block String [SafeProc]

data Function = Function String [String] Int [Block]

newtype Program = Program [Function]

data ConverterState = ConverterState
  { cFunctionsMapping :: M.Map String Int,
    cFunctionsCnt :: Int,
    cCurrentFunction :: Maybe String,
    cFunctions :: M.Map String ([String], Int),
    cResult :: Resolver [[[UncheckedProc]]],
    cCurrentFunctionBlocks :: Resolver [[UncheckedProc]],
    cCurrentBlockInsts :: Resolver [UncheckedProc],
    cBlocksMapping :: M.Map (String, String) Int,
    cBlocksCnt :: Int,
    cVariables :: M.Map String (S.Set String)
  }

type Converter = ST.State ConverterState

enterFunction :: Function -> Converter ()
enterFunction (Function name args retCnt _) = do
  st@(ConverterState {cFunctionsMapping, cFunctionsCnt, cCurrentFunction, cFunctions, cVariables}) <- ST.get
  case cCurrentFunction of
    Just prev -> error $ "Unclosed function " ++ prev
    Nothing -> return ()
  -- FIXME: Why this doesn't work?
  when (M.member name cFunctions) $ error $ "Redefinition of function " ++ name
  ST.put $
    st
      { cFunctionsCnt = cFunctionsCnt + 1,
        cFunctionsMapping = M.insert name cFunctionsCnt cFunctionsMapping,
        cCurrentFunction = Just name,
        cFunctions = M.insert name (args, retCnt) cFunctions,
        cVariables = M.insert name S.empty cVariables
      }
  forM_ (fmap Var args) recordVar
  forM_ (fmap (Var . makeRetVar name) [1 .. retCnt]) recordVar

finishFunction :: Converter ()
finishFunction = do
  st@(ConverterState {cCurrentFunctionBlocks, cResult}) <- ST.get
  currentFunc <- currentFunction
  ST.put $
    st
      { cCurrentFunction = Nothing,
        cCurrentFunctionBlocks = return [],
        cBlocksCnt = 0,
        cResult = do
          cResult' <- cResult
          resolveEnterFunction currentFunc
          func <- cCurrentFunctionBlocks
          resolveFinishFunction
          return $ func : cResult'
      }

currentFunction :: Converter String
currentFunction = do
  ConverterState {cCurrentFunction} <- ST.get
  case cCurrentFunction of
    Just name -> return name
    Nothing -> error "Not inside function"

enterBlock :: Block -> Converter ()
enterBlock (Block name _) = do
  st@(ConverterState {cBlocksMapping, cBlocksCnt}) <- ST.get
  currentFunc <- currentFunction
  when (M.member (currentFunc, name) cBlocksMapping) $ error $ "Redefinition of block " ++ name
  ST.put $
    st
      { cBlocksCnt = cBlocksCnt + 1,
        cBlocksMapping = M.insert (currentFunc, name) cBlocksCnt cBlocksMapping
      }

finishBlock :: Converter ()
finishBlock = do
  st@(ConverterState {cCurrentFunctionBlocks, cCurrentBlockInsts}) <- ST.get
  ST.put $
    st
      { cCurrentBlockInsts = return [],
        cCurrentFunctionBlocks = do
          currentFuncBlocks <- cCurrentFunctionBlocks
          currentInsts <- cCurrentBlockInsts
          return $ currentInsts : currentFuncBlocks
      }

addInstructions :: Resolver [UncheckedProc] -> Converter ()
addInstructions insts = do
  st@(ConverterState {cCurrentBlockInsts}) <- ST.get
  ST.put $
    st
      { cCurrentBlockInsts = do
          block' <- cCurrentBlockInsts
          insts' <- insts
          return $ block' ++ insts'
      }

addInstructionsPure :: [UncheckedProc] -> Converter ()
addInstructionsPure = addInstructions . return

recordVar :: Var -> Converter ()
recordVar (Var name) = do
  st@(ConverterState {cVariables}) <- ST.get
  currentFunc <- currentFunction
  case M.lookup currentFunc cVariables of
    Nothing -> error $ "No variables map for function " ++ currentFunc
    Just vars ->
      ST.put $
        st
          { cVariables = M.insert currentFunc (S.insert name vars) cVariables
          }

data ResolverState = ResolverState
  { rBlocksMapping :: M.Map (String, String) Int,
    rFunctionsMapping :: M.Map String Int,
    rFunctions :: M.Map String ([String], Int),
    rLocalsSize :: M.Map String Int,
    rCurrentFunction :: Maybe String,
    rVarsCnt :: Int,
    rVars :: M.Map String Int
  }

type Resolver = ST.State ResolverState

resolveCurrentFunction :: Resolver String
resolveCurrentFunction = do
  ResolverState {rCurrentFunction} <- ST.get
  case rCurrentFunction of
    Nothing -> error "Not inside function in Resolver"
    Just name -> return name

resolveEnterFunction :: String -> Resolver ()
resolveEnterFunction name = do
  st@(ResolverState {rCurrentFunction, rFunctions}) <- ST.get
  when (isJust rCurrentFunction) $ error $ "Function already set in Resolver when entering " ++ name
  let (args, retSize) = rFunctions M.! name
  ST.put $
    st
      { rCurrentFunction = Just name,
        rVarsCnt = 0,
        rVars = M.empty
      }
  forM_ (fmap Var args) resolveVar
  forM_ (fmap (Var . makeRetVar name) [1 .. retSize]) resolveVar

resolveFinishFunction :: Resolver ()
resolveFinishFunction = do
  st@(ResolverState {rCurrentFunction}) <- ST.get
  when (isNothing rCurrentFunction) $ error "Not inside in function to finish in resolver"
  ST.put $
    st
      { rCurrentFunction = Nothing
      }

resolveVar :: Var -> Resolver UP.Var
resolveVar (Var name) =
  UP.Var <$> do
    st@(ResolverState {rVars, rVarsCnt}) <- ST.get
    case M.lookup name rVars of
      Nothing -> do
        ST.put $
          st
            { rVars = M.insert name rVarsCnt rVars,
              rVarsCnt = rVarsCnt + 1
            }
        return rVarsCnt
      Just idx -> return idx

makeRetVar :: String -> Int -> String
makeRetVar funcName n = "$" ++ funcName ++ "/ret" ++ show n

resolveLbl :: Lbl -> Resolver UP.Lbl
resolveLbl (Lbl name) = do
  ResolverState {rBlocksMapping} <- ST.get
  currentFunc <- resolveCurrentFunction
  return $ UP.Lbl $ rBlocksMapping M.! (currentFunc, name)

data ResolvedFunc = ResolvedFunc {rfIdx :: Int, rfArgsSize :: Int, rfRetSize :: Int}

resolveFunc :: Func -> Resolver ResolvedFunc
resolveFunc (Func name) = do
  ResolverState {rFunctions, rFunctionsMapping} <- ST.get
  let (args, retSize) = rFunctions M.! name
  return $
    ResolvedFunc
      { rfIdx = rFunctionsMapping M.! name,
        rfArgsSize = length args,
        rfRetSize = retSize
      }

resolveLocalsSize :: Resolver Int
resolveLocalsSize = do
  ResolverState {rLocalsSize} <- ST.get
  currentFunc <- resolveCurrentFunction
  return $ rLocalsSize M.! currentFunc

convert :: Program -> [[[UncheckedProc]]]
convert proc =
  let (_, converter) =
        ST.runState (convert' proc) $
          ConverterState
            { cFunctionsMapping = M.empty,
              cFunctionsCnt = 0,
              cCurrentFunction = Nothing,
              cFunctions = M.empty,
              cResult = return [],
              cCurrentFunctionBlocks = return [],
              cCurrentBlockInsts = return [],
              cBlocksMapping = M.empty,
              cBlocksCnt = 0,
              cVariables = M.empty
            }
   in let resolver =
            ResolverState
              { rBlocksMapping = cBlocksMapping converter,
                rFunctionsMapping = cFunctionsMapping converter,
                rFunctions = cFunctions converter,
                rLocalsSize = M.map S.size $ cVariables converter,
                rCurrentFunction = Nothing,
                rVarsCnt = 0,
                rVars = M.empty
              }
       in let (res, _) = ST.runState (cResult converter) resolver
           in reverse (fmap reverse res)
  where
    convert' :: Program -> Converter ()
    convert' (Program functions) = do
      forM_ functions convertFunction'

    convertFunction' :: Function -> Converter ()
    convertFunction' func@(Function _ _ _ blocks) = do
      enterFunction func
      forM_ blocks convertBlock'
      finishFunction

    convertBlock' :: Block -> Converter ()
    convertBlock' block@(Block _ insts) = do
      enterBlock block
      forM_ insts convertInst'
      finishBlock

    convertInst' :: SafeProc -> Converter ()
    convertInst' (SProcGoto lbl) =
      addInstructions $ do
        lbl' <- resolveLbl lbl
        return [ProcGoto lbl']
    convertInst' (SProcBranch var thenLbl elseLbl) = do
      recordVar var
      addInstructions $ do
        var' <- resolveVar var
        thenLbl' <- resolveLbl thenLbl
        elseLbl' <- resolveLbl elseLbl
        return [ProcBranch var' thenLbl' elseLbl']
    convertInst' (SProcConst var n) = do
      recordVar var
      addInstructions $ do
        var' <- resolveVar var
        return [ProcConst var' n]
    convertInst' (SProcCopyAdd var vars) = do
      recordVar var
      forM_ vars recordVar
      addInstructions $ do
        var' <- resolveVar var
        vars' <- forM vars resolveVar
        return [ProcCopyAdd var' vars']
    convertInst' (SProcCopySub var vars) = do
      recordVar var
      forM_ vars recordVar
      addInstructions $ do
        var' <- resolveVar var
        vars' <- forM vars resolveVar
        return [ProcCopySub var' vars']
    convertInst' (SProcRead var) = do
      recordVar var
      addInstructions $ do
        var' <- resolveVar var
        return [ProcRead var']
    convertInst' (SProcWrite var) = do
      recordVar var
      addInstructions $ do
        var' <- resolveVar var
        return [ProcWrite var']
    convertInst' (SProcCall func@(Func name) argVars retVars) = do
      forM_ argVars recordVar
      forM_ retVars recordVar
      addInstructions $ do
        ResolvedFunc {rfIdx, rfArgsSize, rfRetSize} <- resolveFunc func
        unless (rfArgsSize == length argVars) $
          error $
            "Invalid call to function "
              ++ name
              ++ ". Expected "
              ++ show rfArgsSize
              ++ " args, provided "
              ++ show (length argVars)
        unless (rfRetSize == length retVars) $
          error $
            "Invalid call to function "
              ++ name
              ++ ". Expected "
              ++ show rfArgsSize
              ++ " return values, provided "
              ++ show (length retVars)
        localsSize <- resolveLocalsSize
        -- \| Current fame                                    | Callee frame                  | ...
        -- \|                             | localsSize        |             | Locals          | ...
        -- \| frameOffset (Pc, Ret, ... ) | Args | Ret | Vars | frameOffset | Args | Ret | ...| ...
        let shiftSize = frameOffset + localsSize
            -- NOTE: UP.Var when converting will shift index by frameOffset, so subtracting it
            argVarsBase = (shiftSize + frameOffset) - frameOffset
            retVarsBase = argVarsBase + rfArgsSize
        argVars' <- forM argVars resolveVar
        retVars' <- forM retVars resolveVar
        let calleeArgs = [UP.Var $ idx + argVarsBase | idx <- [0 .. rfArgsSize - 1]]
            calleeRets = [UP.Var $ idx + retVarsBase | idx <- [0 .. rfRetSize - 1]]
        return $
          [ProcConst arg 0 | arg <- calleeArgs]
            ++ [ProcCopyAdd var [arg] | (var, arg) <- zip argVars' calleeArgs]
            ++ [ProcCall (UP.Func rfIdx) shiftSize]
            ++ [ProcConst var 0 | var <- retVars']
            ++ [ProcCopyAdd ret [var] | (ret, var) <- zip calleeRets retVars']
    convertInst' (SProcReturn retVars) = do
        forM_ retVars recordVar
        addInstructions $ do
            currentFunc <- resolveCurrentFunction
            ResolvedFunc { rfRetSize } <- resolveFunc (Func currentFunc)
            unless (rfRetSize == length retVars) $
              error $
                "Invalid return in "
                  ++ currentFunc
                  ++ ". Expected "
                  ++ show rfRetSize
                  ++ " return values, provided "
                  ++ show (length retVars)
            let rets = [ Var $ makeRetVar currentFunc idx | idx <- [1 .. rfRetSize] ]
            rets' <- forM rets resolveVar
            retVars' <- forM retVars resolveVar
            return $
                [ ProcConst ret 0 | ret <- rets' ]
                ++ [ ProcCopyAdd var [ret] | (var, ret) <- zip retVars' rets' ]
                ++ [ ProcGoto UP.Ret ]

progToString :: Program -> String
progToString prog = runWriter $ progToString' prog
  where
    progToString' :: Program -> ProgWriter ()
    progToString' (Program functions) =
        forM_ (intersperse nl $ fmap functionToString functions) id

    functionToString :: Function -> ProgWriter ()
    functionToString (Function name args retSize blocks) = do
        write $ "function " ++ name ++ "(" ++ intercalate ", " args ++ ") [" ++ show retSize ++ "] {"
        withIndent $ do
            nl
            forM_ (intersperse nl $ fmap blockToString blocks) id
        nl >> write "}"

    blockToString :: Block -> ProgWriter ()
    blockToString (Block name insts) = do
        write $ name ++ ":"
        withIndent $ do
            nl
            forM_ (intersperse nl $ fmap instToString insts) id

    lblToString :: Lbl -> String
    lblToString (Lbl name) = name

    varToString :: Var -> String
    varToString (Var name) = name

    funcToString :: Func -> String
    funcToString (Func name) = name

    instToString :: SafeProc -> ProgWriter ()
    instToString (SProcGoto lbl) = write $ "goto " ++ lblToString lbl
    instToString (SProcBranch var thenLbl elseLbl) = do
        write $ "if " ++ varToString var ++ " != 0 {"
        withIndent $ nl >> write ("goto " ++ lblToString thenLbl)
        nl >> write "} else {"
        withIndent $ nl >> write ("goto " ++ lblToString elseLbl)
        nl >> write "}"
    instToString (SProcConst var n) = write $ varToString var ++ " := " ++ show n
    instToString (SProcCopyAdd v vs) =
        write $ intercalate "; " (fmap (\v' -> varToString v' ++ " += " ++ varToString v) vs)
    instToString (SProcCopySub v vs) =
        write $ intercalate "; " (fmap (\v' -> varToString v' ++ " -= " ++ varToString v) vs)
    instToString (SProcRead v) = write $ "read " ++ varToString v
    instToString (SProcWrite v) = write $ "write " ++ varToString v
    instToString (SProcCall func args rets) =
        write $ "call " ++ funcToString func ++ "(" ++ intercalate ", " (fmap varToString args) ++ ")"
            ++ " [" ++ intercalate ", " (fmap varToString rets) ++ "]"
    instToString (SProcReturn vars) =
        write $ "return [" ++ intercalate ", " (fmap varToString vars) ++ "]"
