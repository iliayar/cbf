{-# LANGUAGE NamedFieldPuns #-}

module Imp where

import Control.Monad (forM, forM_, replicateM, unless, when)
import qualified Control.Monad.State as ST
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import ProgWriter
import SafeProc (SafeProc (..))
import qualified SafeProc as SP

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
  | StmtCall Func [Expr]

data Function = Function String [(String, Ty)] Ty [Stmt]

data Ty = TyInt | TyArray Ty Int | TyStruct [(String, Ty)] | TyVoid | TyAlias String
  deriving (Eq, Ord)

getArrayElemTy :: Ty -> Ty
getArrayElemTy (TyArray ty _) = ty
getArrayElemTy _ = undefined

data Ref = RefVar Var | RefArray Ref Expr | RefStructField Ref String

data Program = Program [(String, Ty)] [Function]

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
    cTmpVarTys :: M.Map SP.Var Ty,
    cTypeAliases :: M.Map String Ty
  }

type Converter = ST.State ConverterState

convertTy :: Ty -> Converter SP.Ty
convertTy TyInt = return SP.TyInt
convertTy (TyArray ty n) = do
  ty' <- convertTy ty
  return $ SP.TyArray ty' n
convertTy (TyStruct fs) = do
  fs' <- forM fs $ \(name, ty) -> do
    ty' <- convertTy ty
    return (name, ty')
  return $ SP.TyStruct fs'
convertTy TyVoid = return SP.TyVoid
convertTy (TyAlias name) = do
  ConverterState {cTypeAliases} <- ST.get
  let ty = cTypeAliases M.! name
  convertTy ty

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
  st@(ConverterState {cFreeVariables}) <- ST.get
  unless (M.member ty cFreeVariables) $ ST.put $ st {cFreeVariables = M.insert ty S.empty cFreeVariables}

maybeAddAlloc :: SP.Var -> Ty -> Converter ()
maybeAddAlloc var ty = do
  ty' <- convertTy ty
  addInstructions [SProcAlloc var ty']

getTmpVarTy :: SP.Var -> Converter Ty
getTmpVarTy var = do
  ConverterState {cTmpVarTys} <- ST.get
  return $ cTmpVarTys M.! var

acquireTmpVar :: Ty -> Converter SP.Ref
acquireTmpVar ty = do
  initFreeForTy ty
  st@(ConverterState {cFreeVariables, cTmpVariablesCnt, cExprStack, cTmpVarTys}) <- ST.get
  let freeVars = cFreeVariables M.! ty
  case S.lookupMin freeVars of
    Just var -> do
      ST.put $
        st
          { cExprStack = SP.RefVar var : cExprStack,
            cFreeVariables = M.insert ty (S.delete var freeVars) cFreeVariables
          }
      return $ SP.RefVar var
    Nothing -> do
      let var = SP.Var $ "$" ++ show cTmpVariablesCnt
      ST.put $
        st
          { cTmpVariablesCnt = cTmpVariablesCnt + 1,
            cExprStack = SP.RefVar var : cExprStack,
            cTmpVarTys = M.insert var ty cTmpVarTys
          }
      maybeAddAlloc var ty
      return $ SP.RefVar var

popTmpVar :: Converter SP.Ref
popTmpVar = do
  st@(ConverterState {cExprStack}) <- ST.get
  case cExprStack of
    [] -> error "Trying to pop from empty stack"
    (ref : stack') -> do
      ST.put $ st {cExprStack = stack'}
      addFreeVar ref
      return ref

pushTmpVar :: SP.Ref -> Converter ()
pushTmpVar ref = do
  st@(ConverterState {cExprStack}) <- ST.get
  ST.put $
    st
      { cExprStack = ref : cExprStack
      }
  removeFreeVar ref

removeFreeVar :: SP.Ref -> Converter ()
removeFreeVar (SP.RefVar var) = do
  ty <- getTmpVarTy var
  st@(ConverterState {cFreeVariables}) <- ST.get
  let freeVars = cFreeVariables M.! ty
  ST.put $
    st
      { cFreeVariables = M.insert ty (S.delete var freeVars) cFreeVariables
      }
removeFreeVar (SP.RefArrayValue ref) = removeFreeVar ref
removeFreeVar (SP.RefStructField ref _) = removeFreeVar ref

addFreeVar :: SP.Ref -> Converter ()
addFreeVar (SP.RefVar var) = do
  ty <- getTmpVarTy var
  st@(ConverterState {cFreeVariables}) <- ST.get
  let freeVars = cFreeVariables M.! ty
  ST.put $
    st
      { cFreeVariables = M.insert ty (S.insert var freeVars) cFreeVariables
      }
addFreeVar (SP.RefArrayValue ref) = removeFreeVar ref
addFreeVar (SP.RefStructField ref _) = removeFreeVar ref

addVarTy' :: String -> Ty -> Converter ()
addVarTy' name ty = do
  st@(ConverterState {cTypeContext}) <- ST.get
  ST.put $ st {cTypeContext = M.insert name ty cTypeContext}

addVarTy :: Var -> Ty -> Converter ()
addVarTy (Var name) = addVarTy' name

getStructFieldTy :: Ty -> String -> Converter Ty
getStructFieldTy (TyStruct fields) field = return $ findField fields field
  where
    findField ((f, ty) : fs) f' =
      if f == f'
        then ty
        else findField fs f'
    findField [] f = error $ "No such field: " ++ f
getStructFieldTy (TyAlias name) field = do
    ConverterState { cTypeAliases } <- ST.get
    getStructFieldTy (cTypeAliases M.! name) field
getStructFieldTy _ _ = undefined

deriveRefTy :: Ref -> Converter Ty
deriveRefTy (RefVar (Var name)) = do
  ConverterState {cTypeContext} <- ST.get
  case M.lookup name cTypeContext of
    Nothing -> error $ "Unknown variable " ++ name
    Just ty -> return ty
deriveRefTy (RefArray ref _) = do
  ty <- deriveRefTy ref
  return $ getArrayElemTy ty
deriveRefTy (RefStructField ref field) = do
  ty <- deriveRefTy ref
  getStructFieldTy ty field

addFuncTy :: String -> [Ty] -> Ty -> Converter ()
addFuncTy name argTys retTy = do
  st@(ConverterState {cFuncTys}) <- ST.get
  ST.put $ st {cFuncTys = M.insert name (argTys, retTy) cFuncTys}

getSingleRetTy :: Func -> Converter Ty
getSingleRetTy (Func name) = do
  ConverterState {cFuncTys} <- ST.get
  let (_, retTy) = cFuncTys M.! name
  return retTy

data PreparedRef = PreparedRef
  { ref :: SP.Ref,
    focus :: Converter (),
    save :: Converter (),
    discard :: Converter ()
  }

convert :: Program -> SP.Program
convert (Program typeAliases functions) =
  let funcTys = M.fromList $ fmap extractFuncTy functions
   in SP.Program $ fmap (convertFunction funcTys) functions
  where
    extractFuncTy :: Function -> (String, ([Ty], Ty))
    extractFuncTy (Function name args ret _) = (name, (fmap snd args, ret))

    convertFunction :: M.Map String ([Ty], Ty) -> Function -> SP.Function
    convertFunction funcTys (Function name args ret stmts) =
      let initSt =
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
                cTmpVarTys = M.empty,
                cTypeAliases = M.fromList typeAliases
              }
       in let ((args', retTys), ConverterState {cCurrentBody}) =
                ST.runState (do
                    addArgs args
                    convertFunction' stmts
                    args'' <- forM args $ \(name', ty) -> do
                        ty' <- convertTy ty
                        return (name', ty')
                    retTys' <- case ret of
                        TyVoid -> return []
                        ty' -> do
                            ty'' <- convertTy ty'
                            return [ty'']
                    return (args'', retTys')
                ) initSt
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
      return $
        PreparedRef
          { ref = SP.RefVar $ convertVar var,
            focus = return (),
            save = return (),
            discard = return ()
          }
    prepRef (RefArray ref' idxExpr) = do
      PreparedRef {ref, focus, save, discard} <- prepRef ref'
      idx <- convertExpr idxExpr
      pushTmpVar idx
      return $
        PreparedRef
          { ref = SP.RefArrayValue ref,
            focus = do
              focus
              addInstructions [SProcArrayGet ref idx],
            save = do
              addInstructions [SProcArraySet ref idx]
              _ <- popTmpVar
              save,
            discard = do
              _ <- popTmpVar
              discard
          }
    prepRef (RefStructField ref' field) = do
      PreparedRef {ref, focus, save, discard} <- prepRef ref'
      return $
        PreparedRef
          { ref = SP.RefStructField ref field,
            focus = focus,
            save = save,
            discard = discard
          }

    convertStmt :: Stmt -> Converter ()
    convertStmt (StmtAssgn ref' expr) = do
      convertExpr' expr
      tmp <- popTmpVar
      pushTmpVar tmp
      PreparedRef {ref, focus, save} <- prepRef ref'
      focus
      addInstructions [SProcAssign ref tmp]
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
      ty' <- convertTy ty
      addInstructions [SProcAlloc (convertVar var) ty']
    convertStmt (StmtCall (Func "write") [expr]) = do
      tmp <- convertExpr expr
      addInstructions [SProcWrite tmp]
    convertStmt (StmtCall func args) = do
      forM_ args convertExpr'
      vars <- replicateM (length args) popTmpVar
      addInstructions
        [SProcCall (convertFunc func) vars []]

    convertExpr :: Expr -> Converter SP.Ref
    convertExpr expr = do
      convertExpr' expr
      popTmpVar

    convertExpr' :: Expr -> Converter ()
    convertExpr' (ExprRef ref') = do
      ty <- deriveRefTy ref'
      tmp <- acquireTmpVar ty

      PreparedRef {ref, focus, discard} <- prepRef ref'
      focus
      addInstructions [SProcAssign tmp ref]
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
    convertExpr' (ExprCall (Func "read") []) = do
        tmp <- acquireTmpVar TyInt
        addInstructions [SProcRead tmp]
    convertExpr' (ExprCall func args) = do
      forM_ args convertExpr'
      vars <- replicateM (length args) popTmpVar
      ty <- getSingleRetTy func
      tmp <- acquireTmpVar ty
      addInstructions
        [SProcCall (convertFunc func) vars [tmp]]

    convertVar :: Var -> SP.Var
    convertVar (Var v) = SP.Var v

    convertFunc :: Func -> SP.Func
    convertFunc (Func name) = SP.Func name

progToString :: Program -> String
progToString prog = runWriter $ progToString' prog
  where
    progToString' :: Program -> ProgWriter ()
    progToString' (Program typeAliases functions) = do
      forM_ (intersperse nl $ fmap typeAliasToString typeAliases) id
      unless (null typeAliases) nl
      forM_ (intersperse nl $ fmap functionToString functions) id

    typeAliasToString :: (String, Ty) -> ProgWriter ()
    typeAliasToString (name, ty) =
      write $ "typedef " ++ tyToString ty ++ " " ++ name ++ ";"

    argToString :: (String, Ty) -> String
    argToString (name, ty) = tyToString ty ++ " " ++ name

    tyToString :: Ty -> String
    tyToString TyInt = "auto"
    tyToString (TyArray ty n) = tyToString ty ++ "[" ++ show n ++ "]"
    tyToString (TyStruct fs) = "struct { " ++ unwords (fmap (\(f, ty) -> tyToString ty ++ " " ++ f ++ ";") fs) ++ " }"
    tyToString TyVoid = "void"
    tyToString (TyAlias name) = name

    functionToString :: Function -> ProgWriter ()
    functionToString (Function name args ret stmts) = do
      write $ tyToString ret ++ " " ++ name ++ "(" ++ intercalate ", " (fmap argToString args) ++ ") {"
      withIndent $ nl >> stmtsToString stmts
      nl >> write "}"

    stmtsToString :: [Stmt] -> ProgWriter ()
    stmtsToString stmts = forM_ (intersperse nl $ fmap (\stmt -> stmtToString stmt >> write ";") stmts) id

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
    stmtToString (StmtCall func args) = do
      write $ funcToString func ++ "("
      forM_ (intersperse (write ", ") $ fmap exprToString args) id
      write ")"

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
      write "("
      exprToString l
      write " + "
      exprToString r
      write ")"
    exprToString (ExprSub l r) = do
      write "("
      exprToString l
      write " - "
      exprToString r
      write ")"
    exprToString (ExprCall func args) = do
      write $ funcToString func ++ "("
      forM_ (intersperse (write ", ") $ fmap exprToString args) id
      write ")"
