{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module TestSafeProc where

import qualified BasicExt
import Executer (execute)
import SafeProc (Block (..), Func (..), Function (..), Lbl (..), Program (..), SafeProc (..), Var (..))
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
            SafeProc.convert program

mult :: Int -> Int -> Program
mult n m =
  Program
    [ (Function "main" [] 0)
        [ (Block "init")
            [ SProcConst (Var "a") n,
              SProcConst (Var "b") m,
              SProcCall (Func "mult") [Var "a", Var "b"] [Var "c"]
            ]
        ],
      (Function "mult" ["a", "b"] 1)
        [ (Block "init")
            [ SProcConst (Var "ONE") 1,
              SProcConst (Var "res") 0
            ],
          (Block "loop_begin")
            [ SProcBranch (Var "a") (Lbl "loop_body") (Lbl "return")
            ],
          (Block "loop_body")
            [ SProcCopyAdd (Var "b") [Var "res"],
              SProcCopySub (Var "ONE") [Var "a"],
              SProcGoto (Lbl "loop_begin")
            ],
          (Block "return")
            [SProcReturn [Var "res"]]
        ]
    ]

fact :: Int -> Program
fact n =
  Program
    [ (Function "main" [] 0)
      [ (Block "init")
        [ SProcConst (Var "a") n
        , SProcCall (Func "fact") [Var "a"] [Var "a"]
        ]
      ]
    , (Function "mult" ["a", "b"] 1)
        [ (Block "init")
          [ SProcConst (Var "ONE") 1
          , SProcConst (Var "res") 0
          , SProcGoto (Lbl "loop_begin")
          ]
        , (Block "loop_begin")
          [ SProcBranch (Var "a") (Lbl "loop_body") (Lbl "return")
          ]
        , (Block "loop_body")
          [ SProcCopyAdd (Var "b") [Var "res"]
          , SProcCopySub (Var "ONE") [Var "a"]
          , SProcGoto (Lbl "loop_begin")
          ]
        , (Block "return")
          [ SProcReturn [Var "res"] ]
        ]
    , (Function "fact" ["a"] 1)
      [ (Block "init")
        [ SProcBranch (Var "a") (Lbl "step") (Lbl "base")
        ]
      , (Block "step")
        [ SProcConst (Var "ONE") 1
        , SProcConst (Var "b") 0
        , SProcCopyAdd (Var "a") [Var "b"]
        , SProcCopySub (Var "ONE") [Var "b"]
        , SProcCall (Func "fact") [Var "b"] [Var "b"]
        , SProcCall (Func "mult") [Var "a", Var "b"] [Var "b"]
        , SProcReturn [Var "b"]
        ]
      , (Block "base")
        [ SProcConst (Var "ONE") 1
        , SProcReturn [Var "ONE"]
        ]
      ]
    ]

testMult :: Int -> Int -> Int -> Test
testMult n m res = TestCase $ do
  let prog = mult n m
  mem <- evaluate prog
  (mem !! 6) @?= res

testFact :: Int -> Int -> Test
testFact n res = TestCase $ do
  let prog = fact n
  mem <- evaluate prog
  (mem !! 4) @?= res

tests :: Test
tests =
  TestList
    [ testMult 11 11 121,
      testMult 0 42 0,
      testMult 42 0 0,
      testMult 14 1 14,
      testFact 0 1,
      testFact 1 1,
      testFact 4 24,
      testFact 7 176,
      testFact 9 128
    ]
