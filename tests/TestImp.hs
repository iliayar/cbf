{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module TestImp where

import qualified BasicExt
import Executer (execute)
import Imp
import qualified SafeProc
import Test.HUnit
import qualified UncheckedInsts
import qualified UncheckedInstsExt
import qualified UncheckedProc

evaluate :: Program -> IO [Int]
evaluate program =
  execute $
    BasicExt.convert $
      UncheckedInsts.convert $
        UncheckedInstsExt.convert $
          UncheckedProc.convert $
            SafeProc.convert $
              Imp.convert program

kub :: Int -> Program
kub n =
  Program
    [ (Function "main" [] [])
        [ StmtAssgn (Var "res") (ExprCall (Func "kub") [ExprConst n])
        ],
      (Function "mult" [("a", TyInt), ("b", TyInt)] [TyInt])
        [ StmtAssgn (Var "res") (ExprConst 0),
          StmtWhile (ExprVar (Var "a")) 
            [ StmtAssgn (Var "res") (ExprAdd (ExprVar (Var "res")) (ExprVar (Var "b")))
            , StmtAssgn (Var "a") (ExprSub (ExprVar (Var "a")) (ExprConst 1))
            ],
          StmtReturn [ExprVar (Var "res")]
        ],
      (Function "kub" [("a", TyInt)] [TyInt])
        [ StmtReturn [ExprCall (Func "mult") [ExprVar (Var "a"), ExprCall (Func "mult") [ExprVar (Var "a"), ExprVar (Var "a")]]]
        ]
    ]

fact :: Int -> Program
fact n =
  Program
    [ (Function "main" [] [])
        [ StmtAssgn (Var "res") (ExprCall (Func "fact") [ExprConst n])
        ],
      (Function "mult" [("a", TyInt), ("b", TyInt)] [TyInt])
        [ StmtAssgn (Var "res") (ExprConst 0),
          StmtWhile (ExprVar (Var "a")) 
            [ StmtAssgn (Var "res") (ExprAdd (ExprVar (Var "res")) (ExprVar (Var "b")))
            , StmtAssgn (Var "a") (ExprSub (ExprVar (Var "a")) (ExprConst 1))
            ],
          StmtReturn [ExprVar (Var "res")]
        ],
      (Function "fact" [("a", TyInt)] [TyInt])
        [ StmtIf (ExprVar (Var "a"))
            [ StmtReturn [ExprCall (Func "mult") [ExprVar (Var "a"), ExprCall (Func "fact") [ExprSub (ExprVar (Var "a")) (ExprConst 1)]]]
            ]
            [ StmtReturn [ExprConst 1]
            ]
        ]
    ]

testKub :: Int -> Int -> Test
testKub n res = TestCase $ do
  let prog = kub n
  mem <- evaluate prog
  (mem !! 4) @?= res

testFact :: Int -> Int -> Test
testFact n res = TestCase $ do
  let prog = fact n
  mem <- evaluate prog
  (mem !! 4) @?= res

sumArray :: [Int] -> Program
sumArray ns =
  let inits = zipWith (\i v -> StmtAssgnArray (Var "arr") (ExprConst i) (ExprConst v)) [0..] ns in 
  Program
    [ (Function "main" [] []) $
        [ StmtAssgn (Var "res") (ExprConst 0),
          StmtAllocate (Var "arr") $ TyArray TyInt $ length ns
        ] ++ inits ++
        [ StmtAssgn (Var "arr") (ExprCall (Func "inc_all") [ExprVar $ Var "arr"]),
          StmtAssgn (Var "res") (ExprCall (Func "sum") [ExprVar $ Var "arr"])
        ],
      (Function "inc_all" [("arr", TyArray TyInt $ length ns)] [TyArray TyInt $ length ns])
        [ StmtAssgn (Var "i") (ExprConst $ length ns),
          StmtWhile (ExprVar $ Var "i")
            [ StmtAssgn (Var "i") (ExprSub (ExprVar $ Var "i") (ExprConst 1)),
              StmtAssgnArray (Var "arr") (ExprVar $ Var "i") (ExprAdd (ExprArrayGet (Var "arr") (ExprVar $ Var "i")) (ExprConst 1))
            ],
          StmtReturn [ExprVar $ Var "arr"]
        ],
      (Function "sum" [("arr", TyArray TyInt $ length ns)] [TyInt])
        [ StmtAssgn (Var "i") (ExprConst $ length ns),
          StmtAssgn (Var "res") (ExprConst 0),
          StmtWhile (ExprVar $ Var "i")
            [ StmtAssgn (Var "i") (ExprSub (ExprVar $ Var "i") (ExprConst 1)),
              StmtAssgn (Var "res") (ExprAdd (ExprVar $ Var "res") (ExprArrayGet (Var "arr") (ExprVar $ Var "i")))
            ],
          StmtReturn [ExprVar $ Var "res"]
        ]
    ]

testSumArray :: [Int] -> Int -> Test
testSumArray arr res = TestCase $ do
  let prog = sumArray arr
  mem <- evaluate prog
  (mem !! 4) @?= res + length arr

tests :: Test
tests =
  TestList
    [ testKub 11 51,
      testKub 2 8,
      testKub 7 87,
      testFact 0 1,
      testFact 1 1,
      testFact 4 24,
      testFact 7 176,
      testFact 9 128,
      testSumArray [1, 2, 3, 4, 5] 15,
      testSumArray [] 0,
      testSumArray [10] 10,
      testSumArray [10, 15, 33] 58
    ]
