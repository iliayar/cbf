{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module TestSafeProc where

import qualified BasicExt
import Executer (execute)
import SafeProc (Block (..), Func (..), Function (..), Lbl (..), Program (..), SafeProc (..), Var (..), Ty (..))
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
    [ (Function "main" [] [])
        [ (Block "init")
            [ SProcConst (Var "a") n,
              SProcConst (Var "b") m,
              SProcCall (Func "mult") [Var "a", Var "b"] [Var "c"]
            ]
        ],
      (Function "mult" [("a", TyInt), ("b", TyInt)] [TyInt])
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
    [ (Function "main" [] [])
      [ (Block "init")
        [ SProcConst (Var "a") n
        , SProcCall (Func "fact") [Var "a"] [Var "a"]
        ]
      ]
    , (Function "mult" [("a", TyInt), ("b", TyInt)] [TyInt])
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
    , (Function "fact" [("a", TyInt)] [TyInt])
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


sumArray :: [Int] -> Program
sumArray arr =
  let inits = concat $ zipWith initElem [0..] arr in
  Program
    [ (Function "main" [] [])
        [ (Block "init") $
            [ SProcConst (Var "b") 0,
              SProcAlloc (Var "a") $ TyArray TyInt $ length arr
            ] ++ inits ++
            [ SProcCall (Func "inc_all") [Var "a"] [Var "a"],
              SProcCall (Func "sum") [Var "a"] [Var "b"] ]
        ],
      (Function "sum" [("a", TyArray TyInt $ length arr)] [TyInt])
        [ (Block "init")
            [ SProcConst (Var "ONE") 1,
              SProcConst (Var "res") 0,
              SProcConst (Var "i") $ length arr
            ],
          (Block "loop_begin")
            [ SProcBranch (Var "i") (Lbl "loop_body") (Lbl "return")
            ],
          (Block "loop_body")
            [ SProcCopySub (Var "ONE") [Var "i"],
              SProcConst (ArrayTargetVar "a") 0,
              SProcArrayGet (Var "a") (Var "i"),
              SProcCopyAdd (ArrayTargetVar "a") [Var "res"],
              SProcGoto (Lbl "loop_begin")
            ],
          (Block "return")
            [SProcReturn [Var "res"]]
        ],
      (Function "inc_all" [("a", TyArray TyInt $ length arr)] [TyArray TyInt $ length arr])
        [ (Block "init")
            [ SProcConst (Var "ONE") 1,
              SProcConst (Var "i") $ length arr
            ],
          (Block "loop_begin")
            [ SProcBranch (Var "i") (Lbl "loop_body") (Lbl "return") ],
          (Block "loop_body")
            [ SProcCopySub (Var "ONE") [Var "i"],
              SProcConst (ArrayTargetVar "a") 0,
              SProcArrayGet (Var "a") (Var "i"),
              SProcCopyAdd (Var "ONE") [ArrayTargetVar "a"],
              SProcArraySet (Var "a") (Var "i"),
              SProcGoto (Lbl "loop_begin")
            ],
          (Block "return")
            [ SProcReturn [Var "a"] ]
        ]
    ]
    where
        initElem :: Int -> Int -> [SafeProc]
        initElem idx value = 
            [ SProcConst (Var "i") idx,
              SProcConst (ArrayTargetVar "a") value,
              SProcArraySet (Var "a") (Var "i")
            ]

testSumArray :: [Int] -> Int -> Test
testSumArray arr res = TestCase $ do
  let prog = sumArray arr
  mem <- evaluate prog
  (mem !! 4) @?= res + length arr

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
      testFact 9 128,
      testSumArray [1, 2, 3, 4, 5] 15,
      testSumArray [] 0,
      testSumArray [10] 10,
      testSumArray [10, 15, 33] 58
    ]
