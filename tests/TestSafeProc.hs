{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module TestSafeProc where

import qualified BasicExt
import Executer (execute)
import SafeProc (Block (..), Func (..), Function (..), Lbl (..), Program (..), SafeProc (..), Var (..), Ty (..))
import qualified SafeProc as SP
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
            SP.convert program

rv :: String -> SP.Ref
rv s = SP.RefVar $ SP.Var s

mult :: Int -> Int -> Program
mult n m =
  Program
    [ (Function "main" [] [])
        [ (Block "init")
            [ SProcAlloc (Var "a") TyInt,
              SProcAlloc (Var "b") TyInt,
              SProcAlloc (Var "c") TyInt,
              SProcConst (rv "a") n,
              SProcConst (rv "b") m,
              SProcCall (Func "mult") [rv "a", rv "b"] [rv "c"]
            ]
        ],
      (Function "mult" [("a", TyInt), ("b", TyInt)] [TyInt])
        [ (Block "init")
            [ SProcAlloc (Var "ONE") TyInt,
              SProcAlloc (Var "res") TyInt,
              SProcAlloc (Var "a") TyInt,
              SProcAlloc (Var "b") TyInt,
              SProcConst (rv "ONE") 1,
              SProcConst (rv "res") 0
            ],
          (Block "loop_begin")
            [ SProcBranch (rv "a") (Lbl "loop_body") (Lbl "return")
            ],
          (Block "loop_body")
            [ SProcCopyAdd (rv "b") [rv "res"],
              SProcCopySub (rv "ONE") [rv "a"],
              SProcGoto (Lbl "loop_begin")
            ],
          (Block "return")
            [SProcReturn [rv "res"]]
        ]
    ]

fact :: Int -> Program
fact n =
  Program
    [ (Function "main" [] [])
      [ (Block "init")
        [ SProcAlloc (Var "a") TyInt
        , SProcConst (rv "a") n
        , SProcCall (Func "fact") [rv "a"] [rv "a"]
        ]
      ]
    , (Function "mult" [("a", TyInt), ("b", TyInt)] [TyInt])
        [ (Block "init")
          [ SProcAlloc (Var "ONE") TyInt
          , SProcAlloc (Var "res") TyInt
          , SProcAlloc (Var "a") TyInt
          , SProcAlloc (Var "b") TyInt
          , SProcConst (rv "ONE") 1
          , SProcConst (rv "res") 0
          , SProcGoto (Lbl "loop_begin")
          ]
        , (Block "loop_begin")
          [ SProcBranch (rv "a") (Lbl "loop_body") (Lbl "return")
          ]
        , (Block "loop_body")
          [ SProcCopyAdd (rv "b") [rv "res"]
          , SProcCopySub (rv "ONE") [rv "a"]
          , SProcGoto (Lbl "loop_begin")
          ]
        , (Block "return")
          [ SProcReturn [rv "res"] ]
        ]
    , (Function "fact" [("a", TyInt)] [TyInt])
      [ (Block "init")
        [ SProcBranch (rv "a") (Lbl "step") (Lbl "base")
        ]
      , (Block "step")
        [ SProcAlloc (Var "ONE") TyInt
        , SProcAlloc (Var "b") TyInt
        , SProcAlloc (Var "a") TyInt
        , SProcConst (rv "ONE") 1
        , SProcConst (rv "b") 0
        , SProcCopyAdd (rv "a") [rv "b"]
        , SProcCopySub (rv "ONE") [rv "b"]
        , SProcCall (Func "fact") [rv "b"] [rv "b"]
        , SProcCall (Func "mult") [rv "a", rv "b"] [rv "b"]
        , SProcReturn [rv "b"]
        ]
      , (Block "base")
        [ SProcConst (rv "ONE") 1
        , SProcReturn [rv "ONE"]
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
            [ SProcAlloc (Var "b") TyInt,
              SProcAlloc (Var "i") TyInt,
              SProcConst (rv "b") 0,
              SProcAlloc (Var "a") $ TyArray TyInt $ length arr
            ] ++ inits ++
            [ SProcCall (Func "inc_all") [rv "a"] [rv "a"],
              SProcCall (Func "sum") [rv "a"] [rv "b"] ]
        ],
      (Function "sum" [("a", TyArray TyInt $ length arr)] [TyInt])
        [ (Block "init")
            [ SProcAlloc (Var "ONE") TyInt,
              SProcAlloc (Var "res") TyInt,
              SProcAlloc (Var "i") TyInt,
              SProcConst (rv "ONE") 1,
              SProcConst (rv "res") 0,
              SProcConst (rv "i") $ length arr
            ],
          (Block "loop_begin")
            [ SProcBranch (rv "i") (Lbl "loop_body") (Lbl "return")
            ],
          (Block "loop_body")
            [ SProcCopySub (rv "ONE") [rv "i"],
              SProcConst (SP.RefArrayValue $ rv "a") 0,
              SProcArrayGet (rv "a") (rv "i"),
              SProcCopyAdd (SP.RefArrayValue $ rv "a") [rv "res"],
              SProcGoto (Lbl "loop_begin")
            ],
          (Block "return")
            [SProcReturn [rv "res"]]
        ],
      (Function "inc_all" [("a", TyArray TyInt $ length arr)] [TyArray TyInt $ length arr])
        [ (Block "init")
            [ SProcAlloc (Var "ONE") TyInt,
              SProcAlloc (Var "i") TyInt,
              SProcConst (rv "ONE") 1,
              SProcConst (rv "i") $ length arr
            ],
          (Block "loop_begin")
            [ SProcBranch (rv "i") (Lbl "loop_body") (Lbl "return") ],
          (Block "loop_body")
            [ SProcCopySub (rv "ONE") [rv "i"],
              SProcConst (SP.RefArrayValue $ rv "a") 0,
              SProcArrayGet (rv "a") (rv "i"),
              SProcCopyAdd (rv "ONE") [SP.RefArrayValue $ rv "a"],
              SProcArraySet (rv "a") (rv "i"),
              SProcGoto (Lbl "loop_begin")
            ],
          (Block "return")
            [ SProcReturn [rv "a"] ]
        ]
    ]
    where
        initElem :: Int -> Int -> [SafeProc]
        initElem idx value = 
            [ SProcConst (rv "i") idx,
              SProcConst (SP.RefArrayValue $ rv "a") value,
              SProcArraySet (rv "a") (rv "i")
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
