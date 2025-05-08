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
  = ExprVar Var
  | ExprConst Int
  | ExprAdd Expr Expr
  | ExprSub Expr Expr
  | ExprCall Func [Expr]
  | ExprArrayGet Var Expr

data Stmt
  = StmtAssgn Var Expr
  | StmtIf Expr [Stmt] [Stmt]
  | StmtWhile Expr [Stmt]
  | StmtCallAssgn [Var] Func [Expr]
  | StmtReturn [Expr]
  | StmtAllocateArray Var Int
  | StmtAssgnArray Var Expr Expr

data Function = Function String [(String, Ty)] [Ty] [Stmt]

data Ty = TyInt | TyArray Int
    deriving (Eq, Ord)

convertTy :: Ty -> SP.VarType
convertTy TyInt = SP.TyVar
convertTy (TyArray n) = SP.TyArray n

newtype Program = Program [Function]

data ConverterState = ConverterState
  { cCurrentBody :: [SP.Block],
    cBlocksCnt :: Int,
    cCurrentBlockName :: Maybe String,
    cCurrentBlock :: [SafeProc],
    cFreeVariables :: M.Map Ty (S.Set SP.Var),
    cTmpVariablesCnt :: Int,
    cExprStack :: [(SP.Var, Ty)],
    cTypeContext :: M.Map String Ty,
    cFuncTys :: M.Map String ([Ty], [Ty])
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

maybeAllocArray :: SP.Var -> Ty -> Converter ()
maybeAllocArray var (TyArray size) = do
    addInstructions [ SProcArrayAlloc var size ]
maybeAllocArray _ _ = return ()


acquireTmpVar :: Ty -> Converter SP.Var
acquireTmpVar ty = do
    initFreeForTy ty
    st@(ConverterState { cFreeVariables, cTmpVariablesCnt, cExprStack }) <- ST.get
    let freeVars = cFreeVariables M.! ty
    case S.lookupMin freeVars of
        Just var ->  do
            ST.put $ st { 
                cExprStack = (var, ty) : cExprStack,
                cFreeVariables = M.insert ty (S.delete var freeVars) cFreeVariables
            }
            return var
        Nothing -> do
            let var = SP.Var $ "$" ++ show cTmpVariablesCnt
            ST.put $ st { cTmpVariablesCnt = cTmpVariablesCnt + 1, cExprStack = (var, ty) : cExprStack }
            maybeAllocArray var ty
            return var

popTmpVarWithTy :: Converter (SP.Var, Ty)
popTmpVarWithTy = do
    st@(ConverterState { cFreeVariables, cExprStack }) <- ST.get
    case cExprStack of
        [] -> error "Trying to pop from empty stack"
        (var, ty) : stack' -> do
            let freeVars = cFreeVariables M.! ty
            ST.put $ st {
                cFreeVariables = M.insert ty (S.insert var freeVars) cFreeVariables,
                cExprStack = stack'
            }
            return (var, ty)

popTmpVar :: Converter SP.Var
popTmpVar = fst <$> popTmpVarWithTy

pushTmpVar :: (SP.Var, Ty) -> Converter ()
pushTmpVar (var, ty) = do
    st@(ConverterState { cFreeVariables, cExprStack }) <- ST.get
    let freeVars = cFreeVariables M.! ty
    ST.put $ st { 
        cFreeVariables = M.insert ty (S.delete var freeVars) cFreeVariables,
        cExprStack = (var, ty) : cExprStack
    }

addVarTy' :: String -> Ty -> Converter ()
addVarTy' name ty = do
    st@(ConverterState { cTypeContext }) <- ST.get
    ST.put $ st { cTypeContext = M.insert name ty cTypeContext  }

addVarTy :: Var -> Ty -> Converter ()
addVarTy (Var name) = addVarTy' name

-- getVarTy :: String ->  Converter Ty
-- getVarTy name = do
--     ConverterState { cTypeContext } <- ST.get
--     return $ cTypeContext M.! name

getVarTyOrSet :: Var -> Ty ->  Converter Ty
getVarTyOrSet (Var name) ty = do
    st@(ConverterState { cTypeContext }) <- ST.get
    case M.lookup name cTypeContext of
        Just ty' -> return ty'
        Nothing -> do
            ST.put $ st { cTypeContext = M.insert name ty cTypeContext }
            return ty

addFuncTy :: String -> [Ty] -> [Ty] -> Converter ()
addFuncTy name argTys retTys = do
    st@(ConverterState { cFuncTys }) <- ST.get
    ST.put $ st { cFuncTys = M.insert name (argTys, retTys) cFuncTys }

getSingleRetTy :: Func -> Converter Ty
getSingleRetTy (Func name) = do
    ConverterState { cFuncTys } <- ST.get
    let (_, retTys) = cFuncTys M.! name
    case retTys of
        [ret] -> return ret
        _ -> error $ "Function " ++ name ++ " expected to have single ret type"

convert :: Program -> SP.Program
convert (Program functions) = 
    let funcTys = M.fromList $ fmap extractFuncTy functions in
    SP.Program $ fmap (convertFunction funcTys) functions
  where
    extractFuncTy :: Function -> (String, ([Ty], [Ty]))
    extractFuncTy (Function name args rets _) = (name, (fmap snd args, rets))

    convertFunction :: M.Map String ([Ty], [Ty]) -> Function -> SP.Function
    convertFunction funcTys (Function name args rets stmts) =
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
                  cFuncTys = funcTys
                }
          args' = fmap (second convertTy) args
       in SP.Function name args' (fmap convertTy rets) (reverse cCurrentBody)

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

    convertStmt :: Stmt -> Converter ()
    convertStmt (StmtAssgn v expr) = do
      (tmp, ty) <- convertExpr expr
      addVarTy v ty
      addInstructions [ SProcAssign (convertVar v) tmp ]
    convertStmt (StmtIf e thenStmts elseStmts) = do
      thenBlock <- allocateBlock' "thenBranch"
      elseBlock <- allocateBlock' "elseBranch"
      endBlock <- allocateBlock' "ifEnd"
      (tmp, _) <- convertExpr e
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
      (tmp, _) <- convertExpr e
      addInstructions [SProcBranch tmp (SP.Lbl loopBody) (SP.Lbl loopEnd)]
      finishBlock

      startBlock loopBody
      convertStmts body
      addInstructions [SProcGoto (SP.Lbl loopCond)]
      finishBlock

      startBlock loopEnd
    convertStmt (StmtCallAssgn retVars func args) = do
      forM_ args convertExpr'
      -- TODO: Need add these vars to type context
      argVars <- replicateM (length args) popTmpVar
      addInstructions [SProcCall (convertFunc func) (reverse argVars) (fmap convertVar retVars)]
    convertStmt (StmtReturn rets) = do
      forM_ rets convertExpr'
      retVars <- replicateM (length rets) popTmpVar
      addInstructions [SProcReturn (reverse retVars)]
    convertStmt (StmtAllocateArray var size) = do
        addVarTy var (TyArray size)
        addInstructions [SProcArrayAlloc (convertVar var) size]
    convertStmt (StmtAssgnArray (Var name) idxExpr expr) = do
      (value, _) <- convertExpr expr
      addInstructions [ SProcAssign (SP.ArrayTargetVar name) value ]
      (idx, _) <- convertExpr idxExpr
      addInstructions [ SProcArraySet (SP.Var name) idx ]

    convertExpr :: Expr -> Converter (SP.Var, Ty)
    convertExpr expr = do
      convertExpr' expr
      popTmpVarWithTy

    convertExpr' :: Expr -> Converter ()
    convertExpr' (ExprVar v) = do
      ty <- getVarTyOrSet v TyInt
      tmp <- acquireTmpVar ty
      addInstructions [ SProcAssign tmp (convertVar v) ]
    convertExpr' (ExprConst n) = do
      tmp <- acquireTmpVar TyInt
      addInstructions [SProcConst tmp n]
    convertExpr' (ExprAdd l r) = do
      convertExpr' l
      convertExpr' r
      rVar <- popTmpVar
      (lVar, ty) <- popTmpVarWithTy
      addInstructions [SProcCopyAdd rVar [lVar]]
      pushTmpVar (lVar, ty)
    convertExpr' (ExprSub l r) = do
      convertExpr' l
      convertExpr' r
      rVar <- popTmpVar
      (lVar, ty) <- popTmpVarWithTy
      addInstructions [SProcCopySub rVar [lVar]]
      pushTmpVar (lVar, ty)
    convertExpr' (ExprCall func args) = do
        forM_ args convertExpr'
        vars <- replicateM (length args) popTmpVar
        ty <- getSingleRetTy func
        tmp <- acquireTmpVar ty
        addInstructions
          [ SProcCall (convertFunc func) (reverse vars) [tmp] ]
    convertExpr' (ExprArrayGet (Var name) idxExpr) = do
        convertExpr' idxExpr
        idx <- popTmpVar
        addInstructions
          [ SProcConst (SP.ArrayTargetVar name) 0,
            SProcArrayGet (SP.Var name) idx
          ]
        tmp <- acquireTmpVar TyInt
        addInstructions [ SProcAssign tmp (SP.ArrayTargetVar name) ]

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
    argToString (name, ty) = name ++ ": " ++ tyToString ty

    tyToString :: Ty -> String
    tyToString TyInt = "int"
    tyToString (TyArray n) = "int[" ++ show n ++ "]"

    functionToString :: Function -> ProgWriter ()
    functionToString (Function name args rets stmts) = do
      write $ "function " ++ name ++ "(" ++ intercalate ", " (fmap argToString args) ++ ") [" ++ concatMap tyToString rets ++ "] {"
      withIndent $ nl >> stmtsToString stmts
      nl >> write "}"

    stmtsToString :: [Stmt] -> ProgWriter ()
    stmtsToString stmts = forM_ (intersperse nl $ fmap stmtToString stmts) id

    stmtToString :: Stmt -> ProgWriter ()
    stmtToString (StmtAssgn v e) = do
      write $ varToString v ++ " = "
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
    stmtToString (StmtCallAssgn vs func args) = do
      write $ intercalate ", " $ fmap varToString vs
      write $ " = " ++ funcToString func ++ "("
      forM_ (intersperse (write ", ") $ fmap exprToString args) id
      write ")"
    stmtToString (StmtReturn rets) = do
      write "return "
      forM_ (intersperse (write ", ") $ fmap exprToString rets) id
    stmtToString (StmtAllocateArray var size) = do
        write $ "auto " ++ varToString var ++ "[" ++ show size ++ "]"
    stmtToString (StmtAssgnArray arrVar idxExpr expr) = do
        write $ varToString arrVar ++ "["
        exprToString idxExpr
        write "] = "
        exprToString expr

    varToString :: Var -> String
    varToString (Var name) = name

    funcToString :: Func -> String
    funcToString (Func name) = name

    exprToString :: Expr -> ProgWriter ()
    exprToString (ExprVar v) = write $ varToString v
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
    exprToString (ExprArrayGet arrVar idxExpr) = do
        write $ varToString arrVar ++ "["
        exprToString idxExpr
        write "]"
