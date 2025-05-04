{-# LANGUAGE NamedFieldPuns #-}

module Imp where

import Control.Monad (forM_, replicateM, when)
import qualified Control.Monad.State as ST
import Data.List (intercalate, intersperse)
import Data.Maybe (isJust, isNothing)
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
  = ExprVar Var
  | ExprConst Int
  | ExprAdd Expr Expr
  | ExprSub Expr Expr
  | ExprCall Func [Expr]

data Stmt
  = StmtAssgn Var Expr
  | StmtIf Expr [Stmt] [Stmt]
  | StmtWhile Expr [Stmt]
  | StmtCallAssgn [Var] Func [Expr]
  | StmtReturn [Expr]

data Function = Function String [String] Int [Stmt]

newtype Program = Program [Function]

data ConverterState = ConverterState
  { cCurrentBody :: [SP.Block],
    cBlocksCnt :: Int,
    cCurrentBlockName :: Maybe String,
    cCurrentBlock :: [SafeProc],
    cExprStackSize :: Int
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

pushExprStack :: Int -> Converter ()
pushExprStack n = do
  st@(ConverterState {cExprStackSize}) <- ST.get
  ST.put $ st {cExprStackSize = cExprStackSize + n}

popExprStackVar :: Converter SP.Var
popExprStackVar = do
  st@(ConverterState {cExprStackSize}) <- ST.get
  when (cExprStackSize == 0) $ error "Trying pop from empty expr stack"
  ST.put $ st {cExprStackSize = cExprStackSize - 1}
  return $ SP.Var $ "$" ++ show (cExprStackSize - 1)

peekExprStackVar :: Converter SP.Var
peekExprStackVar = do
  ConverterState {cExprStackSize} <- ST.get
  return $ SP.Var $ "$" ++ show cExprStackSize

convert :: Program -> SP.Program
convert (Program functions) = SP.Program $ fmap convertFunction functions
  where
    convertFunction :: Function -> SP.Function
    convertFunction (Function name args retSize stmts) =
      let (_, ConverterState {cCurrentBody}) =
            ST.runState (convertFunction' stmts) $
              ConverterState
                { cCurrentBody = [],
                  cBlocksCnt = 0,
                  cCurrentBlockName = Nothing,
                  cCurrentBlock = [],
                  cExprStackSize = 0
                }
       in SP.Function name args retSize (reverse cCurrentBody)

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
      tmp <- convertExpr expr
      addInstructions
        [ SProcConst (convertVar v) 0,
          SProcCopyAdd tmp [convertVar v]
        ]
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
    convertStmt (StmtCallAssgn retVars func args) = do
      forM_ args convertExpr'
      argVars <- replicateM (length args) popExprStackVar
      addInstructions [SProcCall (convertFunc func) (reverse argVars) (fmap convertVar retVars)]
    convertStmt (StmtReturn rets) = do
      forM_ rets convertExpr'
      retVars <- replicateM (length rets) popExprStackVar
      addInstructions [SProcReturn (reverse retVars)]

    convertExpr :: Expr -> Converter SP.Var
    convertExpr expr = do
      convertExpr' expr
      popExprStackVar

    convertExpr' :: Expr -> Converter ()
    convertExpr' (ExprVar v) = do
      tmp <- peekExprStackVar
      addInstructions
        [ SProcConst tmp 0,
          SProcCopyAdd (convertVar v) [tmp]
        ]
      pushExprStack 1
    convertExpr' (ExprConst n) = do
      tmp <- peekExprStackVar
      addInstructions [SProcConst tmp n]
      pushExprStack 1
    convertExpr' (ExprAdd l r) = do
      convertExpr' l
      convertExpr' r
      rVar <- popExprStackVar
      lVar <- popExprStackVar
      addInstructions [SProcCopyAdd rVar [lVar]]
      pushExprStack 1
    convertExpr' (ExprSub l r) = do
      convertExpr' l
      convertExpr' r
      rVar <- popExprStackVar
      lVar <- popExprStackVar
      addInstructions [SProcCopySub rVar [lVar]]
      pushExprStack 1
    convertExpr' (ExprCall func args) = do
        forM_ args convertExpr'
        vars <- replicateM (length args) popExprStackVar
        tmp <- peekExprStackVar
        addInstructions 
          [ SProcCall (convertFunc func) (reverse vars) [tmp] ]
        pushExprStack 1

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

    functionToString :: Function -> ProgWriter ()
    functionToString (Function name args retSize stmts) = do
      write $ "function " ++ name ++ "(" ++ intercalate ", " args ++ ") [" ++ show retSize ++ "] {"
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
