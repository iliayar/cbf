{-# LANGUAGE NamedFieldPuns #-}

module SafeProc where

import Control.Monad (forM, forM_, unless, when)
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified UncheckedInstsExt as UIE
import qualified UncheckedInsts as UI
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
-- Array variables are NOT introduces in the place of first usage
-- They are introduced by `allocate var N`
--
-- TODO: Check that goto/branch instruction is always the last in the block
-- and every blocks ends with goto/branch. It can be done in UncheckedProc

frameOffset :: Int
frameOffset = case UIE.convertVar $ UP.convertVar (UP.Var 0) of
    UI.Var i -> i

data Var = Var String | ArrayTargetVar String
    deriving (Eq, Ord)

instance Show Var where
    show (Var name) = name
    show (ArrayTargetVar name) = name ++ ".target"

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
  | SProcAssign Var Var
  | SProcAlloc Var Ty
  | SProcArrayGet Var Var
  | SProcArraySet Var Var

data Block = Block String [SafeProc]

data Function = Function String [(String, Ty)] [Ty] [Block]

data Ty = TyInt | TyArray Ty Int
    deriving (Show, Eq)

getArrayElemSize :: Ty -> Int
getArrayElemSize (TyArray ty _) = sizeOfType ty
getArrayElemSize _ = undefined

sizeOfType :: Ty -> Int
sizeOfType TyInt = 1
sizeOfType (TyArray ty n) = UI.arraySize n $ sizeOfType ty

isVarTy :: Ty -> Bool
isVarTy TyInt = True
isVarTy _ = False

isArrayTy :: Ty -> Bool
isArrayTy (TyArray _ _) = True
isArrayTy _ = False

newtype Program = Program [Function]

data ConverterState = ConverterState
  { cFunctionsMapping :: M.Map String Int,
    cFunctionsCnt :: Int,
    cCurrentFunction :: Maybe String,
    cFunctions :: M.Map String ([(String, Ty)], [Ty]),
    cResult :: Resolver [[[UncheckedProc]]],
    cCurrentFunctionBlocks :: Resolver [[UncheckedProc]],
    cCurrentBlockInsts :: Resolver [UncheckedProc],
    cBlocksMapping :: M.Map (String, String) Int,
    cBlocksCnt :: Int,
    cVariables :: M.Map String (M.Map String Ty)
  }

type Converter = ST.State ConverterState

enterFunction :: Function -> Converter ()
enterFunction (Function name args rets _) = do
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
        cFunctions = M.insert name (args, rets) cFunctions,
        cVariables = M.insert name M.empty cVariables
      }
  forM_ args $ \(var, ty) -> recordVar' (Var var) ty
  forM_ (zip [1..] rets) $ \(i, ty) -> recordVar' (Var $ makeRetVar name i) ty

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

recordVar' :: Var -> Ty -> Converter ()
recordVar' (Var name) ty = do
  st@(ConverterState {cVariables}) <- ST.get
  currentFunc <- currentFunction
  case M.lookup currentFunc cVariables of
    Nothing -> error $ "No variables map for function " ++ currentFunc
    Just vars -> 
      case M.lookup name vars of
        Nothing ->
          ST.put $
            st
              { cVariables = M.insert currentFunc (M.insert name ty vars) cVariables
              }
        Just _ -> return ()
recordVar' (ArrayTargetVar _) _ = return ()

recordVar :: Var -> Converter ()
recordVar var = recordVar' var TyInt

data ResolverState = ResolverState
  { rBlocksMapping :: M.Map (String, String) Int,
    rFunctionsMapping :: M.Map String Int,
    rFunctions :: M.Map String ([(String, Ty)], [Ty]),
    rLocalsSize :: M.Map String Int,
    rCurrentFunction :: Maybe String,
    rVarsCnt :: Int,
    rVars :: M.Map String Int,
    rTys :: M.Map String (M.Map String Ty)
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
  let (args, rets) = rFunctions M.! name
  ST.put $
    st
      { rCurrentFunction = Just name,
        rVarsCnt = 0,
        rVars = M.empty
      }
  forM_ args $ \(var, _) -> resolveVar (Var var)
  forM_ (zip [1..] rets) $ \(i, _) -> resolveVar $ Var $ makeRetVar name i

resolveFinishFunction :: Resolver ()
resolveFinishFunction = do
  st@(ResolverState {rCurrentFunction}) <- ST.get
  when (isNothing rCurrentFunction) $ error "Not inside in function to finish in resolver"
  ST.put $
    st
      { rCurrentFunction = Nothing
      }

resolveTy :: Var -> Resolver Ty
resolveTy (Var name) = do
    ResolverState {rTys} <- ST.get
    currentFunc <- resolveCurrentFunction
    return $ case M.lookup name $ rTys M.! currentFunc of
        Nothing -> error $ "Unknown type of variable " ++ name ++ ". Probably array was not allocated"
        Just ty -> ty
resolveTy (ArrayTargetVar name) = do
    _ <- resolveTy (Var name)
    return TyInt

resolveVarSize :: Var -> Resolver Int
resolveVarSize v = sizeOfType <$> resolveTy v

resolveVar :: Var -> Resolver UP.Var
resolveVar v@(Var name) =
  UP.Var <$> do
    st@(ResolverState {rVars, rVarsCnt}) <- ST.get
    case M.lookup name rVars of
      Nothing -> do
        size <- resolveVarSize v
        ST.put $
          st
            { rVars = M.insert name rVarsCnt rVars,
              rVarsCnt = rVarsCnt + size
            }
        return rVarsCnt
      Just idx -> return idx
resolveVar (ArrayTargetVar name) = do
    (v', aTy) <- resolveVarWithTy (Var name)
    case v' of
        UP.Var idx -> return $ UP.ArrTargetVar idx (getArrayElemSize aTy)
        _ -> error "Array expected to resolve in Var"

resolveVarWithTy :: Var -> Resolver (UP.Var, Ty)
resolveVarWithTy v = do
    v' <- resolveVar v
    ty <- resolveTy v
    return (v', ty)

resolveVarWithTyName :: Var -> Resolver (String, UP.Var, Ty)
resolveVarWithTyName v = do
    v' <- resolveVar v
    ty <- resolveTy v
    return (show v, v', ty)

makeRetVar :: String -> Int -> String
makeRetVar funcName n = "$" ++ funcName ++ "/ret" ++ show n

resolveLbl :: Lbl -> Resolver UP.Lbl
resolveLbl (Lbl name) = do
  ResolverState {rBlocksMapping} <- ST.get
  currentFunc <- resolveCurrentFunction
  return $ UP.Lbl $ rBlocksMapping M.! (currentFunc, name)

data ResolvedFunc = ResolvedFunc {rfIdx :: Int, rfArgTys :: [Ty], rfRetTys :: [Ty]}

resolveFunc :: Func -> Resolver ResolvedFunc
resolveFunc (Func name) = do
  ResolverState {rFunctions, rFunctionsMapping} <- ST.get
  let (args, rets) = rFunctions M.! name
  return $
    ResolvedFunc
      { rfIdx = rFunctionsMapping M.! name,
        rfArgTys = fmap snd args,
        rfRetTys = rets
      }

resolveLocalsSize :: Resolver Int
resolveLocalsSize = do
  ResolverState {rLocalsSize} <- ST.get
  currentFunc <- resolveCurrentFunction
  return $ rLocalsSize M.! currentFunc

getLocalsSize :: M.Map String Ty -> Int
getLocalsSize = M.foldl (\a t -> sizeOfType t + a) 0

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
                rLocalsSize = M.map getLocalsSize $ cVariables converter,
                rCurrentFunction = Nothing,
                rVarsCnt = 0,
                rVars = M.empty,
                rTys = cVariables converter
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
        (var', ty) <- resolveVarWithTy var
        unless (isVarTy ty) $ error "Const can be used only with VarTy"
        return [ProcConst var' n]
    convertInst' (SProcCopyAdd var vars) = do
      recordVar var
      forM_ vars recordVar
      addInstructions $ do
        (var', ty) <- resolveVarWithTy var
        vars' <- forM vars resolveVarWithTy
        unless (isVarTy ty && all (isVarTy . snd) vars') $ error "CopyAdd can be used only with VarTy"
        return [ProcCopyAdd var' (fmap fst vars')]
    convertInst' (SProcCopySub var vars) = do
      recordVar var
      forM_ vars recordVar
      addInstructions $ do
        (var', ty) <- resolveVarWithTy var
        vars' <- forM vars resolveVarWithTy
        unless (isVarTy ty && all (isVarTy . snd) vars') $ error "CopySub can be used only with VarTy"
        return [ProcCopySub var' (fmap fst vars')]
    convertInst' (SProcRead var) = do
      recordVar var
      addInstructions $ do
        (var', ty) <- resolveVarWithTy var
        unless (isVarTy ty) $ error "Read can be used only with VarTy"
        return [ProcRead var']
    convertInst' (SProcWrite var) = do
      recordVar var
      addInstructions $ do
        (var', ty) <- resolveVarWithTy var
        unless (isVarTy ty) $ error "Read can be used only with VarTy"
        return [ProcWrite var']
    convertInst' (SProcCall func@(Func name) argVars retVars) = do
      forM_ argVars recordVar
      forM_ retVars recordVar
      addInstructions $ do
        ResolvedFunc {rfIdx, rfArgTys, rfRetTys} <- resolveFunc func
        unless (length rfArgTys == length argVars) $
          error $
            "Invalid call to function "
              ++ name
              ++ ". Expected "
              ++ show (length rfArgTys)
              ++ " args, provided "
              ++ show (length argVars)
        unless (length rfRetTys == length retVars) $
          error $
            "Invalid call to function "
              ++ name
              ++ ". Expected "
              ++ show (length rfRetTys)
              ++ " return values, provided "
              ++ show (length retVars)
        localsSize <- resolveLocalsSize
        -- \| Current fame                                    | Callee frame                  | ...
        -- \|                             | localsSize        |             | Locals          | ...
        -- \| frameOffset (Pc, Ret, ... ) | Args | Ret | Vars | frameOffset | Args | Ret | ...| ...
        let shiftSize = frameOffset + localsSize
            -- NOTE: UP.Var when converting will shift index by frameOffset, so subtracting it
            argVarsBase = (shiftSize + frameOffset) - frameOffset
            retVarsBase = argVarsBase + foldl (\sz ty -> sz + sizeOfType ty) 0 rfArgTys
        argVars' <- forM argVars resolveVarWithTyName
        retVars' <- forM retVars resolveVarWithTyName
        let calleeArgs = 
                fst $ foldr (\ty (res, base) -> (("CALL_ARG", UP.Var base, ty) : res, base + sizeOfType ty))
                    ([], argVarsBase) rfArgTys
            calleeRets = 
                fst $ foldr (\ty (res, base) -> (("CALL_RET", UP.Var base, ty) : res, base + sizeOfType ty))
                    ([], retVarsBase) rfRetTys
        argAssgns <- concat <$> forM (zip calleeArgs argVars') (uncurry makeAssignment')
        retAssgns <- concat <$> forM (zip retVars' calleeRets) (uncurry makeAssignment')
        return $ argAssgns ++ [ProcCall (UP.Func rfIdx) shiftSize] ++ retAssgns
    convertInst' (SProcReturn retVars) = do
        forM_ retVars recordVar
        addInstructions $ do
            currentFunc <- resolveCurrentFunction
            ResolvedFunc { rfRetTys } <- resolveFunc (Func currentFunc)
            unless (length rfRetTys == length retVars) $
              error $
                "Invalid return in "
                  ++ currentFunc
                  ++ ". Expected "
                  ++ show (length rfRetTys)
                  ++ " return values, provided "
                  ++ show (length retVars)
            let rets = [ Var $ makeRetVar currentFunc idx | idx <- [1 .. length rfRetTys] ]
            retAssgns <- concat <$> forM (zip rets retVars) (uncurry makeAssignment)
            return $ retAssgns ++ [ ProcGoto UP.Ret ]
    convertInst' (SProcAssign dv sv) = do
        -- FIXME: Actually its wrong to record these vars as VarTy
        -- Or maybe OK
        recordVar dv
        recordVar sv
        addInstructions $ makeAssignment dv sv
    convertInst' (SProcAlloc var ty) = recordVar' var ty 
    convertInst' (SProcArrayGet av iv) = do
        recordVar iv
        addInstructions $ do
            (iv', iTy) <- resolveVarWithTy iv
            (av', aTy) <- resolveVarWithTy av
            unless (isVarTy iTy) $ error "Index in ArrayGet must be of type VarTy"
            unless (isArrayTy aTy) $ error "Array in ArrayGet must be of type VarTy"
            return [ ProcArrayGet av' iv' (getArrayElemSize aTy) ]
    convertInst' (SProcArraySet av iv) = do
        recordVar iv
        addInstructions $ do
            (iv', iTy) <- resolveVarWithTy iv
            (av', aTy) <- resolveVarWithTy av
            unless (isVarTy iTy) $ error "Index in ArrayGet must be of type VarTy"
            unless (isArrayTy aTy) $ error "Array in ArrayGet must be of type VarTy"
            return [ ProcArraySet av' iv' (getArrayElemSize aTy) ]

    makeAssignment' :: (String, UP.Var, Ty) -> (String, UP.Var, Ty) -> Resolver [UncheckedProc]
    makeAssignment' (dname, dv', dvTy) (sname, sv', svTy) = do
        case (dvTy, svTy) of
            (TyInt, TyInt) -> return
              [ ProcConst dv' 0,
                ProcCopyAdd sv' [dv']
              ]
            (TyArray dveTy dvSize, TyArray sveTy svSize) -> do
                unless (dvSize == svSize && dvTy == svTy) $ 
                    error $ "Cannot assign array " ++ sname ++ " to " ++ dname
                      ++ ". Size {" ++ show sveTy ++ "}" ++ show svSize ++ " not match size {" 
                      ++ show dveTy ++ "}" ++ show dvSize
                return [ ProcArrayCopy sv' dv' dvSize (sizeOfType dveTy) ]
            _ -> error $ "Cannot assign variable " ++ sname ++ " of type " ++ show svTy
                    ++ " to variable " ++ dname ++ " of type " ++ show dvTy

    makeAssignment :: Var -> Var -> Resolver [UncheckedProc]
    makeAssignment dv sv = do
        (dv', dvTy) <- resolveVarWithTy dv
        (sv', svTy) <- resolveVarWithTy sv
        makeAssignment' (show dv, dv', dvTy) (show sv, sv', svTy)

progToString :: Program -> String
progToString prog = runWriter $ progToString' prog
  where
    progToString' :: Program -> ProgWriter ()
    progToString' (Program functions) =
        forM_ (intersperse nl $ fmap functionToString functions) id

    argToString :: (String, Ty) -> String
    argToString (name, ty) = name ++ ": " ++ show ty

    functionToString :: Function -> ProgWriter ()
    functionToString (Function name args rets blocks) = do
        write $ "function " ++ name ++ "(" ++ intercalate ", " (fmap argToString args) ++ ") [" ++ concatMap show rets ++ "] {"
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
    varToString = show

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
    instToString (SProcAlloc var ty) =
        write $ "alloc " ++ varToString var ++ "[" ++ show ty ++ "]"
    instToString (SProcAssign dv sv) =
        write $ varToString dv ++ " = " ++ varToString sv
    instToString (SProcArrayGet av iv) =
        write $ "get " ++ varToString av ++ "[" ++ varToString iv ++ "]"
    instToString (SProcArraySet av iv) =
        write $ "set " ++ varToString av ++ "[" ++ varToString iv ++ "]"
