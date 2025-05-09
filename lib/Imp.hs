{-# LANGUAGE NamedFieldPuns #-}

module Imp where

import Control.Monad (forM_, replicateM, when, unless)
import qualified Control.Monad.State as ST
import Data.List (intercalate, intersperse)
import Data.Maybe (isJust, isNothing)
import ProgWriter
import SafeProc (SafeProc (..))
import qualified SafeProc as SP
import Data.Bifunctor (second)
import qualified Data.Set as S
import qualified Data.Map as M

-- Imperative version of SafeProc. Adds expressions:
-- expr = Var | Const | expr '+' expr | expr '-' expr
--
-- Add statements:
-- Assignments: Var '=' expr
-- Assignment to function call: ( Var )* '=' Func ( expr )*
-- Conditional: if expr { stmts } else { stmts }
-- Loops: while expr { stmts }
--
-- Removes gotos
--
-- var <- [[ expr ]] -- translate expression and yields variable `var`
--
-- [[ stmt ]] -- translate statement(s)
--
-- Var = e
--     |
--     v
-- ...
--
-- v <- [[ expr ]]
-- Var := 0
-- Var += v
--
-- if e { s1 } else { s2 }
--         |
--         v
-- ...
-- v <- [[ expr ]]
-- if v != 0 { goto #then } else { goto #else }
-- #then:
--   [[ s1 ]]
-- #else:
--   [[ s2 ]]
-- #ifend:
--   ...
--
-- while expr { s }
--        |
--        v
-- ...
-- #loop_cond:
--   v <- [[ expr ]]
--   if v != 0 { goto #loop_body } else { goto #loop_end }
-- #loop_body:
--   [[ s ]]
--   goto #loop_cond
-- #loop_end:
--   ...
--
-- Expression translate to RPN, then for each stack cell temporary variable is allocated

newtype Var = Var String

newtype Func = Func String

data Expr
  = ExprRef Ref
  | ExprConst Int
  | ExprAdd Expr Expr
  | ExprSub Expr Expr
  | ExprCall Func [Expr]

data Stmt
  = StmtAssgn Ref Expr
  | StmtIf Expr [Stmt] [Stmt]
  | StmtWhile Expr [Stmt]
  | StmtReturn (Maybe Expr)
  | StmtAllocate Var Ty

data Function = Function String [(String, Ty)] Ty [Stmt]

data Ty = TyInt | TyArray Ty Int | TyStruct [(String, Ty)] | TyVoid
    deriving (Eq, Ord)

convertTy :: Ty -> SP.Ty
convertTy TyInt = SP.TyInt
convertTy (TyArray ty n) = SP.TyArray (convertTy ty) n
convertTy (TyStruct fs) = SP.TyStruct $ fmap (second convertTy) fs
convertTy TyVoid = SP.TyVoid

getArrayElemTy :: Ty -> Ty
getArrayElemTy (TyArray ty _) = ty
getArrayElemTy _ = undefined

data Ref = RefVar Var | RefArray Ref Expr | RefStructField Ref String

newtype Program = Program [Function]

data ConverterState = ConverterState
  { cCurrentBody :: [SP.Block],
    cBlocksCnt :: Int,
    cCurrentBlockName :: Maybe String,
    cCurrentBlock :: [SafeProc],
    cFreeVariables :: M.Map Ty (S.Set SP.Var),
    cTmpVariablesCnt :: Int,
    cExprStack :: [SP.Ref],
    cTypeContext :: M.Map String Ty,
    cFuncTys :: M.Map String ([Ty], Ty),
    cTmpVarTys :: M.Map SP.Var Ty
  }

type Converter = ST.State ConverterState

allocateBlock :: Converter String
allocateBlock = do
  st@(ConverterState {cBlocksCnt}) <- ST.get
  ST.put $ st {cBlocksCnt = cBlocksCnt + 1}
  return $ show cBlocksCnt

allocateBlock' :: String -> Converter String
allocateBlock' prefix = do
    lbl <- allocateBlock
    return $ prefix ++ lbl

finishBlock :: Converter ()
finishBlock = do
  st@(ConverterState {cCurrentBlock, cCurrentBlockName, cCurrentBody}) <- ST.get
  let name = case cCurrentBlockName of
        Nothing -> error "No block to finish"
        Just name' -> name'
  ST.put $ st {cCurrentBody = SP.Block name cCurrentBlock : cCurrentBody}

startBlock :: String -> Converter ()
startBlock name = do
  st@(ConverterState {cCurrentBlockName}) <- ST.get
  when (isJust cCurrentBlockName) $ error "There is an unfinished block, cannot start new"
  ST.put $ st {cCurrentBlockName = Just name, cCurrentBlock = []}

addInstructions :: [SafeProc] -> Converter ()
addInstructions insts = do
  st@(ConverterState {cCurrentBlockName, cCurrentBlock}) <- ST.get
  when (isNothing cCurrentBlockName) $ error "There is no current block, cannot add instructions"
  ST.put $ st {cCurrentBlock = cCurrentBlock ++ insts}

initFreeForTy :: Ty -> Converter ()
initFreeForTy ty = do
    st@(ConverterState { cFreeVariables }) <- ST.get
    unless (M.member ty cFreeVariables) $ ST.put $ st { cFreeVariables = M.insert ty S.empty cFreeVariables }

maybeAddAlloc :: SP.Var -> Ty -> Converter ()
maybeAddAlloc var ty@(TyArray _ _) = do
    addInstructions [ SProcAlloc var $ convertTy ty ]
maybeAddAlloc var ty@(TyStruct _) = do
    addInstructions [ SProcAlloc var $ convertTy ty ]
maybeAddAlloc _ _ = return ()

getTmpVarTy :: SP.Var -> Converter Ty
getTmpVarTy var = do
    ConverterState { cTmpVarTys } <- ST.get
    return $ cTmpVarTys M.! var

acquireTmpVar :: Ty -> Converter SP.Ref
acquireTmpVar ty = do
    initFreeForTy ty
    st@(ConverterState { cFreeVariables, cTmpVariablesCnt, cExprStack, cTmpVarTys }) <- ST.get
    let freeVars = cFreeVariables M.! ty
    case S.lookupMin freeVars of
        Just var ->  do
            ST.put $ st {
                cExprStack = SP.RefVar var : cExprStack,
                cFreeVariables = M.insert ty (S.delete var freeVars) cFreeVariables
            }
            return $ SP.RefVar var
        Nothing -> do
            let var = SP.Var $ "$" ++ show cTmpVariablesCnt
            ST.put $ st {
                cTmpVariablesCnt = cTmpVariablesCnt + 1,
                cExprStack = SP.RefVar var : cExprStack,
                cTmpVarTys = M.insert var ty cTmpVarTys
            }
            maybeAddAlloc var ty
            return $ SP.RefVar var

popTmpVar :: Converter SP.Ref
popTmpVar = do
    st@(ConverterState { cExprStack }) <- ST.get
    case cExprStack of
        [] -> error "Trying to pop from empty stack"
        (ref : stack') -> do
            ST.put $ st { cExprStack = stack' }
            addFreeVar ref
            return ref

pushTmpVar :: SP.Ref -> Converter ()
pushTmpVar ref = do
    st@(ConverterState { cExprStack }) <- ST.get
    ST.put $ st {
        cExprStack = ref : cExprStack
    }
    removeFreeVar ref

removeFreeVar :: SP.Ref -> Converter ()
removeFreeVar (SP.RefVar var) = do
    ty <- getTmpVarTy var
    st@(ConverterState { cFreeVariables }) <- ST.get
    let freeVars = cFreeVariables M.! ty
    ST.put $ st {
        cFreeVariables = M.insert ty (S.delete var freeVars) cFreeVariables
    }
removeFreeVar (SP.RefArrayValue ref) = removeFreeVar ref
removeFreeVar (SP.RefStructField ref _) = removeFreeVar ref

addFreeVar :: SP.Ref -> Converter ()
addFreeVar (SP.RefVar var) = do
    ty <- getTmpVarTy var
    st@(ConverterState { cFreeVariables }) <- ST.get
    let freeVars = cFreeVariables M.! ty
    ST.put $ st {
        cFreeVariables = M.insert ty (S.insert var freeVars) cFreeVariables
    }
addFreeVar (SP.RefArrayValue ref) = removeFreeVar ref
addFreeVar (SP.RefStructField ref _) = removeFreeVar ref

addVarTy' :: String -> Ty -> Converter ()
addVarTy' name ty = do
    st@(ConverterState { cTypeContext }) <- ST.get
    ST.put $ st { cTypeContext = M.insert name ty cTypeContext  }

addVarTy :: Var -> Ty -> Converter ()
addVarTy (Var name) = addVarTy' name

setNewVarTy :: Ref -> Ty -> Converter ()
setNewVarTy (RefVar (Var name)) ty = do
    st@(ConverterState { cTypeContext }) <- ST.get
    case M.lookup name cTypeContext of
        Just _ -> return ()
        Nothing -> do
            ST.put $ st { cTypeContext = M.insert name ty cTypeContext }
setNewVarTy _ _ = return ()

getStructFieldTy :: Ty -> String -> Ty
getStructFieldTy (TyStruct fields) field = findField fields field
    where
        findField ((f, ty) : fs) f' =
          if f == f' then ty
          else findField fs f'
        findField [] f = error $ "No such field: " ++ f
getStructFieldTy _ _ = undefined

deriveRefTy :: Ref -> Converter Ty
deriveRefTy (RefVar (Var name)) = do
    ConverterState { cTypeContext } <- ST.get
    return $ cTypeContext M.! name
deriveRefTy (RefArray ref _) = do
    ty <- deriveRefTy ref
    return $ getArrayElemTy ty
deriveRefTy (RefStructField ref field) = do
    ty <- deriveRefTy ref
    return $ getStructFieldTy ty field

addFuncTy :: String -> [Ty] -> Ty -> Converter ()
addFuncTy name argTys retTy = do
    st@(ConverterState { cFuncTys }) <- ST.get
    ST.put $ st { cFuncTys = M.insert name (argTys, retTy) cFuncTys }

getSingleRetTy :: Func -> Converter Ty
getSingleRetTy (Func name) = do
    ConverterState { cFuncTys } <- ST.get
    let (_, retTy) = cFuncTys M.! name
    return retTy

data PreparedRef = PreparedRef {
    ref :: SP.Ref,
    focus :: Converter (),
    save :: Converter (),
    discard :: Converter ()
}

convert :: Program -> SP.Program
convert (Program functions) =
    let funcTys = M.fromList $ fmap extractFuncTy functions in
    SP.Program $ fmap (convertFunction funcTys) functions
  where
    extractFuncTy :: Function -> (String, ([Ty], Ty))
    extractFuncTy (Function name args ret _) = (name, (fmap snd args, ret))

    convertFunction :: M.Map String ([Ty], Ty) -> Function -> SP.Function
    convertFunction funcTys (Function name args ret stmts) =
      let (_, ConverterState {cCurrentBody}) =
            ST.runState (addArgs args >> convertFunction' stmts) $
              ConverterState
                { cCurrentBody = [],
                  cBlocksCnt = 0,
                  cCurrentBlockName = Nothing,
                  cCurrentBlock = [],
                  cExprStack = [],
                  cFreeVariables = M.empty,
                  cTmpVariablesCnt = 0,
                  cTypeContext = M.empty,
                  cFuncTys = funcTys,
                  cTmpVarTys = M.empty
                }
          args' = fmap (second convertTy) args
      in
      let retTys = case ret of
            TyVoid -> []
            ty -> [convertTy ty]
      in SP.Function name args' retTys (reverse cCurrentBody)

    addArgs :: [(String, Ty)] -> Converter ()
    addArgs args = forM_ args (uncurry addVarTy')

    convertFunction' :: [Stmt] -> Converter ()
    convertFunction' stmts = do
      initBlock <- allocateBlock' "init"
      startBlock initBlock
      convertStmts stmts
      finishBlock

    convertStmts :: [Stmt] -> Converter ()
    convertStmts stmts = forM_ stmts convertStmt

    prepRef :: Ref -> Converter PreparedRef
    prepRef (RefVar var) = do
        return $ PreparedRef {
            ref = SP.RefVar $ convertVar var,
            focus = return (),
            save = return (),
            discard = return ()
        }
    prepRef (RefArray ref' idxExpr) = do
        PreparedRef { ref, focus, save, discard } <- prepRef ref'
        idx <- convertExpr idxExpr
        pushTmpVar idx
        return $ PreparedRef {
            ref = SP.RefArrayValue ref,
            focus = do
                focus
                addInstructions [ SProcArrayGet ref idx ],
            save = do
                addInstructions [ SProcArraySet ref idx ]
                _ <- popTmpVar
                save,
            discard = do
                _ <- popTmpVar
                discard
        }
    prepRef (RefStructField ref' field) = do
        PreparedRef { ref, focus, save, discard } <- prepRef ref'
        return $ PreparedRef {
            ref = SP.RefStructField ref field,
            focus = focus,
            save = save,
            discard = discard
        }


    convertStmt :: Stmt -> Converter ()
    convertStmt (StmtAssgn ref' expr) = do
      convertExpr' expr
      tmp <- popTmpVar
      pushTmpVar tmp
      PreparedRef { ref, focus, save } <- prepRef ref'
      focus
      addInstructions [ SProcAssign ref tmp ]
      save
      _ <- popTmpVar
      return ()
    convertStmt (StmtIf e thenStmts elseStmts) = do
      thenBlock <- allocateBlock' "thenBranch"
      elseBlock <- allocateBlock' "elseBranch"
      endBlock <- allocateBlock' "ifEnd"
      tmp <- convertExpr e
      addInstructions
        [SProcBranch tmp (SP.Lbl thenBlock) (SP.Lbl elseBlock)]
      finishBlock

      startBlock thenBlock
      convertStmts thenStmts
      addInstructions [SProcGoto (SP.Lbl endBlock)]
      finishBlock

      startBlock elseBlock
      convertStmts elseStmts
      -- NOTE: Goto not needed
      finishBlock

      startBlock endBlock
    convertStmt (StmtWhile e body) = do
      loopCond <- allocateBlock' "whileCondition"
      loopBody <- allocateBlock' "whileBody"
      loopEnd <- allocateBlock' "whileEnd"

      finishBlock

      startBlock loopCond
      tmp <- convertExpr e
      addInstructions [SProcBranch tmp (SP.Lbl loopBody) (SP.Lbl loopEnd)]
      finishBlock

      startBlock loopBody
      convertStmts body
      addInstructions [SProcGoto (SP.Lbl loopCond)]
      finishBlock

      startBlock loopEnd
    convertStmt (StmtReturn (Just ret)) = do
      retVar <- convertExpr ret
      addInstructions [SProcReturn [retVar]]
    convertStmt (StmtReturn Nothing) = do
      addInstructions [SProcReturn []]
    convertStmt (StmtAllocate var ty) = do
        addVarTy var ty
        addInstructions [SProcAlloc (convertVar var) $ convertTy ty]

    convertExpr :: Expr -> Converter SP.Ref
    convertExpr expr = do
      convertExpr' expr
      popTmpVar

    convertExpr' :: Expr -> Converter ()
    convertExpr' (ExprRef ref') = do
      setNewVarTy ref' TyInt
      ty <- deriveRefTy ref'
      tmp <- acquireTmpVar ty

      PreparedRef { ref, focus, discard } <- prepRef ref'
      focus
      addInstructions [ SProcAssign tmp ref ]
      discard
    convertExpr' (ExprConst n) = do
      tmp <- acquireTmpVar TyInt
      addInstructions [SProcConst tmp n]
    convertExpr' (ExprAdd l r) = do
      convertExpr' l
      convertExpr' r
      rVar <- popTmpVar
      lVar <- popTmpVar
      addInstructions [SProcCopyAdd rVar [lVar]]
      pushTmpVar lVar
    convertExpr' (ExprSub l r) = do
      convertExpr' l
      convertExpr' r
      rVar <- popTmpVar
      lVar <- popTmpVar
      addInstructions [SProcCopySub rVar [lVar]]
      pushTmpVar lVar
    convertExpr' (ExprCall func args) = do
        forM_ args convertExpr'
        vars <- replicateM (length args) popTmpVar
        ty <- getSingleRetTy func
        tmp <- acquireTmpVar ty
        addInstructions
          [ SProcCall (convertFunc func) (reverse vars) [tmp] ]

    convertVar :: Var -> SP.Var
    convertVar (Var v) = SP.Var v

    convertFunc :: Func -> SP.Func
    convertFunc (Func name) = SP.Func name

progToString :: Program -> String
progToString prog = runWriter $ progToString' prog
  where
    progToString' :: Program -> ProgWriter ()
    progToString' (Program functions) =
      forM_ (intersperse nl $ fmap functionToString functions) id

    argToString :: (String, Ty) -> String
    argToString (name, ty) = tyToString ty ++ " " ++ name

    tyToString :: Ty -> String
    tyToString TyInt = "auto"
    tyToString (TyArray ty n) = tyToString ty ++ "[" ++ show n ++ "]"
    tyToString (TyStruct fs) = "struct { " ++ unwords (fmap (\(f, ty) -> tyToString ty ++ " " ++ f ++ ";") fs) ++ " }"
    tyToString TyVoid = "void"

    functionToString :: Function -> ProgWriter ()
    functionToString (Function name args ret stmts) = do
      write $ tyToString ret ++ " " ++ name ++ "(" ++ intercalate ", " (fmap argToString args) ++ ") {"
      withIndent $ nl >> stmtsToString stmts
      nl >> write "}"

    stmtsToString :: [Stmt] -> ProgWriter ()
    stmtsToString stmts = forM_ (intersperse nl $ fmap stmtToString stmts) id

    stmtToString :: Stmt -> ProgWriter ()
    stmtToString (StmtAssgn ref e) = do
      refToString ref
      write " = "
      exprToString e
    stmtToString (StmtIf e thenStmts elseStmts) = do
      write "if "
      exprToString e
      write " != 0 {"
      withIndent $ nl >> stmtsToString thenStmts
      nl >> write "} else {"
      withIndent $ nl >> stmtsToString elseStmts
      nl >> write "}"
    stmtToString (StmtWhile e body) = do
      write "while "
      exprToString e
      write " != 0 {"
      withIndent $ nl >> stmtsToString body
      nl >> write "}"
    stmtToString (StmtReturn (Just ret)) = do
      write "return "
      exprToString ret
    stmtToString (StmtReturn Nothing) = do
      write "return"
    stmtToString (StmtAllocate var ty) = do
        write $ tyToString ty ++ " " ++ varToString var

    varToString :: Var -> String
    varToString (Var name) = name

    funcToString :: Func -> String
    funcToString (Func name) = name

    refToString :: Ref -> ProgWriter ()
    refToString (RefVar v) = write $ varToString v
    refToString (RefArray ref idx) = do
        refToString ref
        write "["
        exprToString idx
        write "]"
    refToString (RefStructField ref field) = do
        refToString ref
        write $ "." ++ field

    exprToString :: Expr -> ProgWriter ()
    exprToString (ExprRef ref) = refToString ref
    exprToString (ExprConst n) = write $ show n
    exprToString (ExprAdd l r) = do
      exprToString l
      write " + "
      exprToString r
    exprToString (ExprSub l r) = do
      exprToString l
      write " - "
      exprToString r
    exprToString (ExprCall func args) = do
      write $ funcToString func ++ "("
      forM_ (intersperse (write ", ") $ fmap exprToString args) id
      write ")"
