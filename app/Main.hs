{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Main where

import Basic (Brainfuck, bfFromString, bfToString)
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

example :: Program
example =
  Program
    [ (Function "main" [] 0)
        [ StmtAssgn (Var "res") (ExprCall (Func "fact") [ExprConst 7])
        ],
      (Function "mult" ["a", "b"] 1)
        [ StmtAssgn (Var "res") (ExprConst 0),
          StmtWhile (ExprVar (Var "a")) 
            [ StmtAssgn (Var "res") (ExprAdd (ExprVar (Var "res")) (ExprVar (Var "b")))
            , StmtAssgn (Var "a") (ExprSub (ExprVar (Var "a")) (ExprConst 1))
            ],
          StmtReturn [ExprVar (Var "res")]
        ],
      (Function "fact" ["a"] 1)
        [ StmtIf (ExprVar (Var "a"))
            [ StmtReturn [ExprCall (Func "mult") [ExprVar (Var "a"), ExprCall (Func "fact") [ExprSub (ExprVar (Var "a")) (ExprConst 1)]]]
            ]
            [ StmtReturn [ExprConst 1]
            ]
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
      putStrLn "UncheckedProc:"
      putStrLn $ UncheckedProc.progToString procProc
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
      evaluate procBf
    filename : _ -> do
      content <- readFile filename
      evaluate $ bfFromString content
