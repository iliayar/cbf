{-# LANGUAGE NamedFieldPuns #-}
module Imp.Parser where

import Imp
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad (void, unless, when)
import Control.Monad.Combinators.Expr
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Data.Maybe (isJust)
import qualified Data.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ImpState = ImpState {
    stTypeDefs :: M.Map String Ty,
    stFuncDeclsWithoutBody :: M.Map String Function,
    stFuncDecls :: M.Map String Function,
    stVariables :: M.Map String Ty
}

type ImpParser = ParsecT Custom String (State ImpState)

data Custom
  = UndeclaredType String
  | TypeRedefinition String
  | FunctionRedfinition String
  | UndeclaredVariable String
  | UndeclaredFunction String
  | VariableRedefinition String
  | NoMain
    deriving (Eq, Ord)

instance ShowErrorComponent Custom where
    showErrorComponent (UndeclaredType t) = "Undeclared type " ++ t
    showErrorComponent (FunctionRedfinition t) = "Function " ++ t ++ " redefinition"
    showErrorComponent (TypeRedefinition t) = "Type " ++ t ++ " redefinition"
    showErrorComponent (VariableRedefinition t) = "Variable " ++ t ++ " redefinition"
    showErrorComponent (UndeclaredVariable t) = "Undeclared variable " ++ t
    showErrorComponent (UndeclaredFunction t) = "Undeclared function " ++ t
    showErrorComponent NoMain = "main function is node defined"

ws :: ImpParser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

identifier :: ImpParser String
identifier = (:) <$> firstChar <*> many nonFirstChar
    where
        firstChar = letterChar <|> char '_'
        nonFirstChar = digitChar <|> firstChar

ty :: ImpParser Ty
ty = choice
  [ TyInt <$ string "int",
    TyVoid <$ string "void",
    structTy,
    typeAlias
  ]

impFail :: Int -> Custom -> ImpParser a
impFail o err = parseError $ FancyError o $ S.fromList [ErrorCustom err]

typeAlias :: ImpParser Ty
typeAlias = do
    o <- getOffset
    ident <- identifier
    ImpState { stTypeDefs } <- get
    unless (M.member ident stTypeDefs) $ impFail o $ UndeclaredType ident
    return $ TyAlias ident

int :: ImpParser Int
int = do
    n <- some digitChar
    return $ read n

maybeArrayDecl :: Ty -> ImpParser Ty
maybeArrayDecl ty' = do
    sz <- optional $ char '[' *> ws *> int <* ws <* char ']'
    case sz of
        Just sz' -> maybeArrayDecl $ TyArray ty' sz'
        Nothing -> return ty'

varDecl' :: Bool -> ImpParser (String, Ty)
varDecl' doCheck = do
    tyBase <- ty <* ws
    -- o <- getOffset
    ident <- identifier <* ws
    ty' <- maybeArrayDecl tyBase

    when doCheck $ do
      st@(ImpState { stVariables }) <- get
      -- when (M.member ident stVariables) $ impFail o $ VariableRedefinition ident
      put $ st { stVariables = M.insert ident ty' stVariables }

    return (ident, ty')

varDeclChecking :: ImpParser (String, Ty)
varDeclChecking = varDecl' True

varDecl :: ImpParser (String, Ty)
varDecl = varDecl' False

structTy :: ImpParser Ty
structTy = do
    string "struct" >> ws
    char '{' >> ws
    fields <- some $ varDecl <* char ';' <* ws
    void $ char '}'
    return $ TyStruct fields

typeDefDecl :: ImpParser (String, Ty)
typeDefDecl = do
    string "typedef" >> ws
    o <- getOffset
    (name, ty') <- varDecl <* ws
    void $ char ';'

    st@(ImpState { stTypeDefs }) <- get
    when (M.member name stTypeDefs) $ impFail o $ TypeRedefinition name
    put $ st { stTypeDefs = M.insert name ty' stTypeDefs }

    return (name, ty')

funcDecl :: ImpParser (Maybe Function)
funcDecl = do
    retTy <- ty <* ws
    o <- getOffset
    funcName <- identifier <* ws
    args <- char '(' *> ws *> sepBy (varDecl <* ws) (char ',' >> ws) <* ws <* char ')' <* ws
    brace <- optional $ char '{' <* ws
    case brace of
        Nothing -> do
            void $ char ';'
            modify $ \st -> st {
                stFuncDeclsWithoutBody =
                    M.insert funcName (Function funcName args retTy []) $ stFuncDeclsWithoutBody st
                }
            return Nothing
        Just _ -> do
            -- FIXME: Check that arguments has different names
            modify $ \st -> st { stVariables = M.fromList args }
            stmts <- statements <* ws
            void $ char '}'

            st@(ImpState { stFuncDecls }) <- get
            when (M.member funcName stFuncDecls) $ impFail o $ FunctionRedfinition funcName

            let func = Function funcName args retTy stmts
            put $ st { stFuncDecls = M.insert funcName func stFuncDecls }
            return $ Just func


funcCall :: ImpParser (Func, [Expr])
funcCall = do
    o <- getOffset
    ident <- identifier <* ws
    args <- char '(' *> ws *> sepBy (expr <* ws) (char ',' <* ws) <* char ')'

    ImpState { stFuncDecls, stFuncDeclsWithoutBody } <- get
    unless (M.member ident stFuncDecls
        || M.member ident stFuncDeclsWithoutBody
        || ident == "read" || ident == "write") $
        impFail o $ UndeclaredFunction ident

    return (Func ident, args)

exprCall :: ImpParser Expr
exprCall = uncurry ExprCall <$> funcCall

expr :: ImpParser Expr
expr = makeExprParser (ws *> term <* ws) table
    where
        binary :: String -> (Expr -> Expr -> Expr) -> Operator ImpParser Expr
        binary s f = InfixL $ f <$ string s

        table =
          [ [ binary "+" ExprAdd,
              binary "-" ExprSub
            ]
          ]

        term = choice
          [ ExprConst <$> int,
            ExprConst . Data.Char.ord <$> charLit,
            paren,
            try exprCall,
            ExprRef <$> impRef
          ]

        paren = char '(' *> ws *> expr <* ws <* char ')'

        charLit :: ImpParser Char
        charLit = do
            ch <- char '\'' *> anySingle
            case ch of 
                '\\' -> do
                    specialCh <- anySingle <* char '\''
                    case specialCh of
                        'n' -> return '\n'
                        't' -> return '\t'
                        'r' -> return '\r'
                        _ -> error $ "Unknown special symbol " ++ [specialCh]
                _ -> char '\'' >> return ch

maybeArrRef :: Ref -> ImpParser (Maybe Ref)
maybeArrRef r = do
    lb <- ws *> optional (char '[') <* ws
    case lb of
        Nothing -> return Nothing
        Just _ -> do
            idxExpr <- expr <* ws <* char ']'
            Just <$> impRef' (RefArray r idxExpr)

maybeFieldRef :: Ref -> ImpParser (Maybe Ref)
maybeFieldRef r = do
    dot <- ws *> optional (char '.') <* ws
    case dot of
        Nothing -> return Nothing
        Just _ -> do
            ident <- identifier
            Just <$> impRef' (RefStructField r ident)

impRef' :: Ref -> ImpParser Ref
impRef' r = do
    arrRef <- maybeArrRef r
    case arrRef of
        Nothing -> do
            structRef <- maybeFieldRef r
            case structRef of
                Nothing -> return r
                Just r' -> return r'
        Just r' -> return r'

impRef :: ImpParser Ref
impRef = do
    o <- getOffset
    ident <- identifier
    checkDefined o ident
    impRef' $ RefVar $ Var ident

stmtCall :: ImpParser [Stmt]
stmtCall = (\(fun, args) -> [StmtCall fun args]) <$> funcCall <* ws <* char ';'

stmtAlloc :: ImpParser [Stmt]
stmtAlloc = do
    (varName, varTy) <- varDeclChecking <* ws
    assgn <- optional $ char '=' <* ws
    case assgn of
        Nothing -> do
            void $ ws <* char ';'
            return [StmtAllocate (Var varName) varTy]
        Just _ -> do
            initExpr <- expr <* ws <* char ';'
            return
              [ StmtAllocate (Var varName) varTy,
                StmtAssgn (RefVar $ Var varName) initExpr
              ]

stmtReturn :: ImpParser [Stmt]
stmtReturn = do
    void $ string "return" <* ws
    retExpr <- optional expr <* ws <* char ';'
    return [StmtReturn retExpr]

stmtWhile :: ImpParser [Stmt]
stmtWhile = do
    void $ string "while" <* ws <* char '(' <* ws
    condExpr <- expr <* ws
    void $ char ')' <* ws <* char '{' <* ws
    bodyStmts <- statements <* ws <* char '}'
    return [StmtWhile condExpr bodyStmts]

stmtIf :: ImpParser [Stmt]
stmtIf = do
    void $ string "if" <* ws <* char '(' <* ws
    condExpr <- expr <* ws
    void $ char ')' <* ws <* char '{' <* ws
    thenBody <- statements <* ws <* char '}' <* ws
    void $ string "else" <* ws <* char '{' <* ws
    elseBody <- statements <* char '}'
    return [StmtIf condExpr thenBody elseBody]

stmtAssgn :: ImpParser [Stmt]
stmtAssgn = do
    ref' <- impRef
    void $ ws *> char '=' <* ws
    expr' <- expr <* ws <* char ';'
    return [StmtAssgn ref' expr']

checkDefined :: Int -> String -> ImpParser ()
checkDefined o ident = do
    ImpState { stVariables } <- get
    unless (M.member ident stVariables) $ impFail o $ UndeclaredVariable ident

statement :: ImpParser [Stmt]
statement = choice
  [ stmtWhile,
    stmtIf,
    stmtReturn,
    try stmtCall,
    try stmtAssgn,
    stmtAlloc
  ]

statements :: ImpParser [Stmt]
statements = do
    stmts <- many (statement <* ws)
    return $ concat stmts

data Decl = TypeDef (String, Ty) | FuncDefOrDecl (Maybe Function)

decl :: ImpParser Decl
decl = choice
  [ TypeDef <$> typeDefDecl,
    FuncDefOrDecl <$> funcDecl
  ]

splitDecls :: [Decl] -> ([(String, Ty)], [Function])
splitDecls [] = ([], [])
splitDecls (TypeDef def : ds) =
    let (typedefs, funcs) = splitDecls ds in
    (def : typedefs, funcs)
splitDecls (FuncDefOrDecl Nothing : ds) = splitDecls ds
splitDecls (FuncDefOrDecl (Just func) : ds) =
    let (typedefs, funcs) = splitDecls ds in
    (typedefs, func : funcs)

extractMain :: [Function] -> (Maybe Function, [Function])
extractMain [] = (Nothing, [])
extractMain (f@(Function name _ _ _) : fs) =
    let (mMain, fs') = extractMain fs in
    if name == "main" then
        if isJust mMain then error "Duplicate main"
        else (Just f, fs')
    else (mMain, f : fs')

moveMainToTop :: [Function] -> ImpParser [Function]
moveMainToTop fs = do
    let (main, fs') = extractMain fs
    case main of
        Nothing -> do
            o <- getOffset
            impFail o NoMain
        Just main' -> return $ main' : fs'

program :: ImpParser Program
program = do
    decls <- ws *> many (decl <* ws)
    let (typedefs, funcs) = splitDecls decls
    funcs' <- moveMainToTop funcs
    return $ Program typedefs funcs'

runImpParser :: String -> String -> Either (ParseErrorBundle String Custom) Program
runImpParser filename s = 
    let (res, _) = runState (runParserT program filename s) ImpState {
        stTypeDefs = M.empty,
        stFuncDeclsWithoutBody = M.empty,
        stFuncDecls = M.empty,
        stVariables = M.empty
    } in res

parseProgramUnsafe :: String -> Program
parseProgramUnsafe s = case runImpParser "" s of
    Left err -> error $ "Failed to parse:\n" ++ errorBundlePretty err
    Right res -> res

parseProgram :: String -> String -> IO (Maybe Program)
parseProgram filename s =
    let res = runImpParser filename s in
    case res  of
        Left err -> do
            putStrLn $ errorBundlePretty err
            return Nothing
        Right res' -> return $ Just res'
