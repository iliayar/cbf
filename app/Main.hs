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

-- example :: Program
-- example =
--   Program
--     [ (Function "main" [] 0)
--         [ StmtAssgn (Var "res") (ExprCall (Func "fact") [ExprConst 10])
--         ],
--       (Function "mult" ["a", "b"] 1)
--         [ StmtAssgn (Var "res") (ExprConst 0),
--           StmtWhile (ExprVar (Var "a")) 
--             [ StmtAssgn (Var "res") (ExprAdd (ExprVar (Var "res")) (ExprVar (Var "b")))
--             , StmtAssgn (Var "a") (ExprSub (ExprVar (Var "a")) (ExprConst 1))
--             ],
--           StmtReturn [ExprVar (Var "res")]
--         ],
--       (Function "fact" ["a"] 1)
--         [ StmtIf (ExprVar (Var "a"))
--             [ StmtReturn [ExprCall (Func "mult") [ExprVar (Var "a"), ExprCall (Func "fact") [ExprSub (ExprVar (Var "a")) (ExprConst 1)]]]
--             ]
--             [ StmtReturn [ExprConst 1]
--             ]
--         ]
--     ]

fact :: Int -> Program
fact n =
  Program
    [ (Function "main" [] [])
        [ StmtAssgn (Var "res") (ExprConst 0),
          StmtAllocateArray (Var "arr") 5,
          StmtAssgnArray (Var "arr") (ExprConst 0) (ExprConst 1),
          StmtAssgnArray (Var "arr") (ExprConst 1) (ExprConst 2),
          StmtAssgnArray (Var "arr") (ExprConst 2) (ExprConst 3),
          StmtAssgnArray (Var "arr") (ExprConst 3) (ExprConst 4),
          StmtAssgnArray (Var "arr") (ExprConst 4) (ExprConst 5),
          StmtAssgn (Var "arr") (ExprCall (Func "inc_all") [ExprVar $ Var "arr"]),
          StmtAssgn (Var "res") (ExprCall (Func "sum") [ExprVar $ Var "arr"])
        ],
      (Function "inc_all" [("arr", TyArray 5)] [TyArray 5])
        [ StmtAssgn (Var "i") (ExprConst 5),
          StmtWhile (ExprVar $ Var "i")
            [ StmtAssgn (Var "i") (ExprSub (ExprVar $ Var "i") (ExprConst 1)),
              StmtAssgnArray (Var "arr") (ExprVar $ Var "i") (ExprAdd (ExprArrayGet (Var "arr") (ExprVar $ Var "i")) (ExprConst 1))
            ],
          StmtReturn [ExprVar $ Var "arr"]
        ],
      (Function "sum" [("arr", TyArray 5)] [TyInt])
        [ StmtAssgn (Var "i") (ExprConst 5),
          StmtAssgn (Var "res") (ExprConst 0),
          StmtWhile (ExprVar $ Var "i")
            [ StmtAssgn (Var "i") (ExprSub (ExprVar $ Var "i") (ExprConst 1)),
              StmtAssgn (Var "res") (ExprAdd (ExprVar $ Var "res") (ExprArrayGet (Var "arr") (ExprVar $ Var "i")))
            ],
          StmtReturn [ExprVar $ Var "res"]
        ]
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      let procImp = fact 3
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
