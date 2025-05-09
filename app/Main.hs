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

sumArray :: [Int] -> Program
sumArray ns =
  let inits = zipWith (\i v -> StmtAssgnArray (Var "arr") (ExprConst i) (ExprConst v)) [0..] ns in 
  Program
    [ (Function "main" [] []) $
        [ StmtAssgn (RefVar $ Var "res") (ExprConst 0),
          StmtAllocate (Var "arr") $ TyArray TyInt $ length ns
        ] ++ inits ++
        [ StmtAssgn (RefVar $ Var "arr") (ExprCall (Func "inc_all") [ExprVar $ Var "arr"]),
          StmtAssgn (RefVar $ Var "res") (ExprCall (Func "sum") [ExprVar $ Var "arr"])
        ],
      (Function "inc_all" [("arr", TyArray TyInt $ length ns)] [TyArray TyInt $ length ns])
        [ StmtAssgn (RefVar $ Var "i") (ExprConst $ length ns),
          StmtWhile (ExprVar $ Var "i")
            [ StmtAssgn (RefVar $ Var "i") (ExprSub (ExprVar $ Var "i") (ExprConst 1)),
              StmtAssgn (RefArray (RefVar $ Var "arr") (ExprVar $ Var "i")) (ExprAdd (ExprArrayGet (Var "arr") (ExprVar $ Var "i")) (ExprConst 1))
              -- StmtAssgnArray (Var "arr") (ExprVar $ Var "i") (ExprAdd (ExprArrayGet (Var "arr") (ExprVar $ Var "i")) (ExprConst 1))
            ],
          StmtReturn [ExprVar $ Var "arr"]
        ],
      (Function "sum" [("arr", TyArray TyInt $ length ns)] [TyInt])
        [ StmtAssgn (RefVar $ Var "i") (ExprConst $ length ns),
          StmtAssgn (RefVar $ Var "res") (ExprConst 0),
          StmtWhile (ExprVar $ Var "i")
            [ StmtAssgn (RefVar $ Var "i") (ExprSub (ExprVar $ Var "i") (ExprConst 1)),
              StmtAssgn (RefVar $ Var "res") (ExprAdd (ExprVar $ Var "res") (ExprArrayGet (Var "arr") (ExprVar $ Var "i")))
            ],
          StmtReturn [ExprVar $ Var "res"]
        ]
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      let procImp = sumArray [1, 2, 3]
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
