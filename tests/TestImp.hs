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
    [ (Function "main" [] 0)
        [ StmtAssgn (Var "res") (ExprCall (Func "kub") [ExprConst n])
        ],
      (Function "mult" ["a", "b"] 1)
        [ StmtAssgn (Var "res") (ExprConst 0),
          StmtWhile (ExprVar (Var "a")) 
            [ StmtAssgn (Var "res") (ExprAdd (ExprVar (Var "res")) (ExprVar (Var "b")))
            , StmtAssgn (Var "a") (ExprSub (ExprVar (Var "a")) (ExprConst 1))
            ],
          StmtReturn [ExprVar (Var "res")]
        ],
      (Function "kub" ["a"] 1)
        [ StmtReturn [ExprCall (Func "mult") [ExprVar (Var "a"), ExprCall (Func "mult") [ExprVar (Var "a"), ExprVar (Var "a")]]]
        ]
    ]

fact :: Int -> Program
fact n =
  Program
    [ (Function "main" [] 0)
        [ StmtAssgn (Var "res") (ExprCall (Func "fact") [ExprConst n])
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
      testFact 9 128
    ]
