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

newtype Var = Var String
    deriving (Eq, Ord)

instance Show Var where
    show (Var name) = name

data Ref = RefVar Var | RefArrayValue Ref | RefStructField Ref String

instance Show Ref where
    show (RefVar var) = show var
    show (RefArrayValue ref) = show ref ++ ".value"
    show (RefStructField ref f) = show ref ++ "." ++ f

newtype Lbl = Lbl String

newtype Func = Func String

data SafeProc
  = SProcGoto Lbl
  | SProcBranch Ref Lbl Lbl
  | SProcConst Ref Int
  | SProcCopyAdd Ref [Ref]
  | SProcCopySub Ref [Ref]
  | SProcRead Ref
  | SProcWrite Ref
  | SProcCall Func [Ref] [Ref]
  | SProcReturn [Ref]
  | SProcAssign Ref Ref
  | SProcAlloc Var Ty
  | SProcArrayGet Ref Ref
  | SProcArraySet Ref Ref

data Block = Block String [SafeProc]

data Function = Function String [(String, Ty)] [Ty] [Block]

data Ty = TyInt | TyArray Ty Int | TyStruct [(String, Ty)]
    deriving (Show, Eq)

getArrayElemTy :: Ty -> Ty
getArrayElemTy (TyArray ty _) = ty
getArrayElemTy _ = undefined

getArrayElemSize :: Ty -> Int
getArrayElemSize ty = sizeOfType $ getArrayElemTy ty

getStructFields :: Ty -> [(String, Ty)]
getStructFields (TyStruct fs) = fs
getStructFields _ = undefined

sizeOfType :: Ty -> Int
sizeOfType TyInt = 1
sizeOfType (TyArray ty n) = UI.arraySize n $ sizeOfType ty
sizeOfType (TyStruct fs) = foldl (\acc (_, ty) -> acc + sizeOfType ty) 0 fs

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
  forM_ args $ \(var, ty) -> recordVar' (RefVar $ Var var) ty
  forM_ (zip [1..] rets) $ \(i, ty) -> recordVar' (RefVar $ Var $ makeRetVar name i) ty

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

recordVar' :: Ref -> Ty -> Converter ()
recordVar' (RefVar (Var name)) ty = do
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
recordVar' (RefArrayValue _) _ = return ()
recordVar' (RefStructField _ _) _ = return ()

recordVar :: Ref -> Converter ()
recordVar ref = recordVar' ref TyInt

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

findFieldOffsetAndTy :: [(String, Ty)] -> String -> (Int, Ty)
findFieldOffsetAndTy = findFieldOffsetAndTy' 0
    where
        findFieldOffsetAndTy' :: Int -> [(String, Ty)] -> String -> (Int, Ty)
        findFieldOffsetAndTy' o ((f, ty) : fs) f' =
          if f == f' then (o, ty)
          else findFieldOffsetAndTy' (o + sizeOfType ty) fs f'
        findFieldOffsetAndTy' _ [] f = error $ "No such field: " ++ f

resolveRefWithTyName :: Ref -> Resolver (String, UP.Ref, Ty)
resolveRefWithTyName (RefVar var) = do
    (name, var', ty) <- resolveVarWithTyName var
    return (name, UP.RefVar var', ty)
resolveRefWithTyName (RefArrayValue ref) = do
    (name, ref', ty) <- resolveRefWithTyName ref
    let eTy = getArrayElemTy ty
    return (name ++ ".value", UP.RefArrayValue ref' $ sizeOfType eTy, eTy)
resolveRefWithTyName (RefStructField ref field) = do
    (name, ref', ty) <- resolveRefWithTyName ref
    let (o, ty') = findFieldOffsetAndTy (getStructFields ty) field
    return (name ++ "." ++ field, UP.RefStructField ref' o, ty')

resolveRefWithTy :: Ref -> Resolver (UP.Ref, Ty)
resolveRefWithTy ref = do
    (_, ref', ty) <- resolveRefWithTyName ref
    return (ref', ty)

resolveRef :: Ref -> Resolver UP.Ref
resolveRef ref = do
    (ref', _) <- resolveRefWithTy ref
    return ref'

resolveVarWithTyName :: Var -> Resolver (String, UP.Var, Ty)
resolveVarWithTyName (Var name) = do
    st@(ResolverState {rVars, rVarsCnt, rTys}) <- ST.get
    currentFunc <- resolveCurrentFunction
    let ty = case M.lookup name $ rTys M.! currentFunc of
          Nothing -> error $ "Unknown type of variable " ++ name ++ ". Probably array was not allocated"
          Just ty' -> ty'
    var <- case M.lookup name rVars of
      Nothing -> do
        ST.put $
          st
            { rVars = M.insert name rVarsCnt rVars,
              rVarsCnt = rVarsCnt + sizeOfType ty
            }
        return rVarsCnt
      Just idx -> return idx
    return (name, UP.Var var, ty)

resolveVar :: Var -> Resolver UP.Var
resolveVar v = do
    (v', _) <- resolveVarWithTy v
    return v'

resolveVarWithTy :: Var -> Resolver (UP.Var, Ty)
resolveVarWithTy v = do
    (_, v', ty) <- resolveVarWithTyName v
    return (v', ty)

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
    convertInst' (SProcBranch ref thenLbl elseLbl) = do
      recordVar ref
      addInstructions $ do
        ref' <- resolveRef ref
        thenLbl' <- resolveLbl thenLbl
        elseLbl' <- resolveLbl elseLbl
        return [ProcBranch ref' thenLbl' elseLbl']
    convertInst' (SProcConst ref n) = do
      recordVar ref
      addInstructions $ do
        (ref', ty) <- resolveRefWithTy ref
        unless (isVarTy ty) $ error "Const can be used only with VarTy"
        return [ProcConst ref' n]
    convertInst' (SProcCopyAdd ref refs) = do
      recordVar ref
      forM_ refs recordVar
      addInstructions $ do
        (ref', ty) <- resolveRefWithTy ref
        refs' <- forM refs resolveRefWithTy
        unless (isVarTy ty && all (isVarTy . snd) refs') $ error "CopyAdd can be used only with VarTy"
        return [ProcCopyAdd ref' (fmap fst refs')]
    convertInst' (SProcCopySub ref refs) = do
      recordVar ref
      forM_ refs recordVar
      addInstructions $ do
        (ref', ty) <- resolveRefWithTy ref
        refs' <- forM refs resolveRefWithTy
        unless (isVarTy ty && all (isVarTy . snd) refs') $ error "CopySub can be used only with VarTy"
        return [ProcCopySub ref' (fmap fst refs')]
    convertInst' (SProcRead ref) = do
      recordVar ref
      addInstructions $ do
        (ref', ty) <- resolveRefWithTy ref
        unless (isVarTy ty) $ error "Read can be used only with VarTy"
        return [ProcRead ref']
    convertInst' (SProcWrite ref) = do
      recordVar ref
      addInstructions $ do
        (ref', ty) <- resolveRefWithTy ref
        unless (isVarTy ty) $ error "Read can be used only with VarTy"
        return [ProcWrite ref']
    convertInst' (SProcCall func@(Func name) argRefs retRefs) = do
      forM_ argRefs recordVar
      forM_ retRefs recordVar
      addInstructions $ do
        ResolvedFunc {rfIdx, rfArgTys, rfRetTys} <- resolveFunc func
        unless (length rfArgTys == length argRefs) $
          error $
            "Invalid call to function "
              ++ name
              ++ ". Expected "
              ++ show (length rfArgTys)
              ++ " args, provided "
              ++ show (length argRefs)
        unless (length rfRetTys == length retRefs) $
          error $
            "Invalid call to function "
              ++ name
              ++ ". Expected "
              ++ show (length rfRetTys)
              ++ " return values, provided "
              ++ show (length retRefs)
        localsSize <- resolveLocalsSize
        -- \| Current fame                                    | Callee frame                  | ...
        -- \|                             | localsSize        |             | Locals          | ...
        -- \| frameOffset (Pc, Ret, ... ) | Args | Ret | Vars | frameOffset | Args | Ret | ...| ...
        let shiftSize = frameOffset + localsSize
            -- NOTE: UP.Var when converting will shift index by frameOffset, so subtracting it
            argVarsBase = (shiftSize + frameOffset) - frameOffset
            retVarsBase = argVarsBase + foldl (\sz ty -> sz + sizeOfType ty) 0 rfArgTys
        argRefs' <- forM argRefs resolveRefWithTyName
        retRefs' <- forM retRefs resolveRefWithTyName
        let calleeArgs =
                fst $ foldr (\ty (res, base) -> (("CALL_ARG", UP.RefVar $ UP.Var base, ty) : res, base + sizeOfType ty))
                    ([], argVarsBase) rfArgTys
            calleeRets =
                fst $ foldr (\ty (res, base) -> (("CALL_RET", UP.RefVar $ UP.Var base, ty) : res, base + sizeOfType ty))
                    ([], retVarsBase) rfRetTys
        argAssgns <- concat <$> forM (zip calleeArgs argRefs') (uncurry makeAssignment')
        retAssgns <- concat <$> forM (zip retRefs' calleeRets) (uncurry makeAssignment')
        return $ argAssgns ++ [ProcCall (UP.Func rfIdx) shiftSize] ++ retAssgns
    convertInst' (SProcReturn retRefs) = do
        forM_ retRefs recordVar
        addInstructions $ do
            currentFunc <- resolveCurrentFunction
            ResolvedFunc { rfRetTys } <- resolveFunc (Func currentFunc)
            unless (length rfRetTys == length retRefs) $
              error $
                "Invalid return in "
                  ++ currentFunc
                  ++ ". Expected "
                  ++ show (length rfRetTys)
                  ++ " return values, provided "
                  ++ show (length retRefs)
            let rets = [ RefVar $ Var $ makeRetVar currentFunc idx | idx <- [1 .. length rfRetTys] ]
            retAssgns <- concat <$> forM (zip rets retRefs) (uncurry makeAssignment)
            return $ retAssgns ++ [ ProcGoto UP.Ret ]
    convertInst' (SProcAssign dr sr) = do
        -- FIXME: Actually its wrong to record these vars as VarTy
        -- Or maybe OK
        recordVar dr
        recordVar sr
        addInstructions $ makeAssignment dr sr
    convertInst' (SProcAlloc var ty) = recordVar' (RefVar var) ty
    convertInst' (SProcArrayGet ar ir) = do
        recordVar ir
        addInstructions $ do
            (ir', iTy) <- resolveRefWithTy ir
            (ar', aTy) <- resolveRefWithTy ar
            unless (isVarTy iTy) $ error "Index in ArrayGet must be of type VarTy"
            unless (isArrayTy aTy) $ error "Array in ArrayGet must be of type VarTy"
            return [ ProcArrayGet ar' ir' (getArrayElemSize aTy) ]
    convertInst' (SProcArraySet ar ir) = do
        recordVar ir
        addInstructions $ do
            (ir', iTy) <- resolveRefWithTy ir
            (ar', aTy) <- resolveRefWithTy ar
            unless (isVarTy iTy) $ error "Index in ArrayGet must be of type VarTy"
            unless (isArrayTy aTy) $ error "Array in ArrayGet must be of type VarTy"
            return [ ProcArraySet ar' ir' (getArrayElemSize aTy) ]

    makeAssignment' :: (String, UP.Ref, Ty) -> (String, UP.Ref, Ty) -> Resolver [UncheckedProc]
    makeAssignment' (dname, dr', drTy) (sname, sr', srTy) = do
        case (drTy, srTy) of
            (TyInt, TyInt) -> return
              [ ProcConst dr' 0,
                ProcCopyAdd sr' [dr']
              ]
            (TyArray dreTy drSize, TyArray sreTy srSize) -> do
                unless (drSize == srSize && drTy == srTy) $
                    error $ "Cannot assign array " ++ sname ++ " to " ++ dname
                      ++ ". Size {" ++ show sreTy ++ "}" ++ show srSize ++ " not match size {"
                      ++ show dreTy ++ "}" ++ show drSize
                return [ ProcArrayCopy sr' dr' drSize (sizeOfType dreTy) ]
            (TyStruct dFields, TyStruct sFields) -> do
                unless (dFields == sFields) $
                  error $ "Cannot assign struct variable " ++ sname ++ " of type " ++ show srTy
                     ++ " to struct variable " ++ dname ++ " of type " ++ show drTy
                makeStructAssignments 0 dFields sFields
            _ -> error $ "Cannot assign variable " ++ sname ++ " of type " ++ show srTy
                    ++ " to variable " ++ dname ++ " of type " ++ show drTy
        where
            makeStructAssignments o ((dF, dTy) : dFs) ((sF, sTy) : sFs) = do
                fAss <- makeAssignment'
                  (dname ++ "." ++ dF, UP.RefStructField dr' o, dTy)
                  (sname ++ "." ++ sF, UP.RefStructField sr' o, sTy)
                fOther <- makeStructAssignments (o + sizeOfType dTy) dFs sFs
                return $ fAss ++ fOther
            makeStructAssignments _ [] [] = return []
            makeStructAssignments _ _ _ = undefined

    makeAssignment :: Ref -> Ref -> Resolver [UncheckedProc]
    makeAssignment dr sr = do
        (dr', drTy) <- resolveRefWithTy dr
        (sr', srTy) <- resolveRefWithTy sr
        makeAssignment' (show dr, dr', drTy) (show sr, sr', srTy)

progToString :: Program -> String
progToString prog = runWriter $ progToString' prog
  where
    progToString' :: Program -> ProgWriter ()
    progToString' (Program functions) =
        forM_ (intersperse nl $ fmap functionToString functions) id

    tyToString :: Ty -> String
    tyToString TyInt = "auto"
    tyToString (TyArray ty n) = tyToString ty ++ "[" ++ show n ++ "]"
    tyToString (TyStruct fs) = "struct { " ++ unwords (fmap (\(f, ty) -> tyToString ty ++ " " ++ f ++ ";") fs) ++ " }"

    argToString :: (String, Ty) -> String
    argToString (name, ty) = tyToString ty ++ " " ++ name

    functionToString :: Function -> ProgWriter ()
    functionToString (Function name args rets blocks) = do
        write $ "function " ++ name ++ "(" ++ intercalate ", " (fmap argToString args) ++ ") [" ++ concatMap tyToString rets ++ "] {"
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

    refToString = show

    instToString :: SafeProc -> ProgWriter ()
    instToString (SProcGoto lbl) = write $ "goto " ++ lblToString lbl
    instToString (SProcBranch ref thenLbl elseLbl) = do
        write $ "if " ++ refToString ref ++ " != 0 {"
        withIndent $ nl >> write ("goto " ++ lblToString thenLbl)
        nl >> write "} else {"
        withIndent $ nl >> write ("goto " ++ lblToString elseLbl)
        nl >> write "}"
    instToString (SProcConst ref n) = write $ refToString ref ++ " := " ++ show n
    instToString (SProcCopyAdd r rs) =
        write $ intercalate "; " (fmap (\r' -> refToString r' ++ " += " ++ refToString r) rs)
    instToString (SProcCopySub r rs) =
        write $ intercalate "; " (fmap (\r' -> refToString r' ++ " -= " ++ refToString r) rs)
    instToString (SProcRead r) = write $ "read " ++ refToString r
    instToString (SProcWrite r) = write $ "write " ++ refToString r
    instToString (SProcCall func args rets) =
        write $ "call " ++ funcToString func ++ "(" ++ intercalate ", " (fmap refToString args) ++ ")"
            ++ " [" ++ intercalate ", " (fmap refToString rets) ++ "]"
    instToString (SProcReturn refs) =
        write $ "return [" ++ intercalate ", " (fmap refToString refs) ++ "]"
    instToString (SProcAlloc var ty) =
        write $ "alloc " ++ varToString var ++ " " ++ tyToString ty
    instToString (SProcAssign dr sr) =
        write $ refToString dr ++ " = " ++ refToString sr
    instToString (SProcArrayGet ar ir) =
        write $ "get " ++ refToString ar ++ "[" ++ refToString ir ++ "]"
    instToString (SProcArraySet ar ir) =
        write $ "set " ++ refToString ar ++ "[" ++ refToString ir ++ "]"
