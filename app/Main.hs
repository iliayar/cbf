{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Main where

import Basic (Brainfuck, bfFromString, bfToString, doubleCellSize)
import qualified BasicExt
import Executer (execute)
import Imp
import qualified SafeProc
import System.Environment (getArgs)
import qualified UncheckedInsts
import qualified UncheckedInstsExt
import qualified UncheckedProc

evaluate :: [Brainfuck] -> IO ()
evaluate program = do
  putStrLn $ "Evaluating: " ++ bfToString program
  mem <- execute program
  putStrLn ""
  putStrLn $ "Memory: " ++ show mem

re :: String -> Expr
re s = ExprRef $ RefVar $ Var s

example :: Program
example =
  let sTy = TyStruct [("a", TyInt), ("b", TyArray TyInt 5), ("c", TyInt)] in
  Program
    [ (Function "main" [] TyVoid)
        [ StmtAssgn (RefVar $ Var "res") (ExprConst 42),
          StmtAllocate (Var "s") sTy,
          StmtAssgn (RefStructField (RefVar $ Var "s") "a") $ ExprConst 10,
          StmtAssgn (RefVar $ Var "res") (ExprRef $ RefStructField (RefVar $ Var "s") "a"),
          StmtAssgn (RefArray (RefStructField (RefVar $ Var "s") "b") (ExprConst 1)) $ ExprConst 20,
          StmtAssgn (RefArray (RefStructField (RefVar $ Var "s") "b") (ExprConst 0)) $ ExprConst 21,
          StmtAssgn (RefStructField (RefVar $ Var "s") "c") $ ExprConst 121,
          StmtAssgn (RefVar $ Var "s") (ExprCall (Func "modify_struct") [ExprRef $ RefVar $ Var "s"])
        ],
      (Function "modify_struct" [("s", sTy)] sTy)
        [ StmtAssgn (RefArray (RefStructField (RefVar $ Var "s") "b") (ExprConst 2)) $ ExprConst 19,
          StmtAssgn (RefStructField (RefVar $ Var "s") "a") $ ExprConst 74,
          StmtReturn $ Just $ ExprRef $ RefVar $ Var "s"
        ]
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      let procImp = example
      putStrLn "Imp:"
      putStrLn $ Imp.progToString procImp
      let procSafeProc = Imp.convert procImp
      putStrLn "SafeProc:"
      putStrLn $ SafeProc.progToString procSafeProc
      let procProc = SafeProc.convert procSafeProc
      -- putStrLn "UncheckedProc:"
      -- putStrLn $ UncheckedProc.progToString procProc
      let procInstsExt = UncheckedProc.convert procProc
      -- putStrLn "UncheckedInstExt:"
      -- putStrLn $ UncheckedInstsExt.progToString procInstsExt
      let procInsts = UncheckedInstsExt.convert procInstsExt
      -- putStrLn "UncheckedInst:"
      -- putStrLn $ UncheckedInsts.progToString procInsts
      let procBfExt = BasicExt.optimize $ UncheckedInsts.convert procInsts
      -- putStrLn "BasicExt:"
      -- putStrLn $ BasicExt.progToString procBfExt
      let procBf = BasicExt.convert procBfExt
      -- evaluate $ doubleCellSize procBf
      evaluate procBf
    filename : _ -> do
      content <- readFile filename
      evaluate $ bfFromString content
