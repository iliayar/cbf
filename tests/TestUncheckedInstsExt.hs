module TestUncheckedInstsExt where

import qualified BasicExt
import Executer (execute)
import Test.HUnit
import qualified UncheckedInsts
import UncheckedInstsExt (Lbl (..), UncheckedInstExt (..), Var (..))
import qualified UncheckedInstsExt

evaluate :: [[UncheckedInstExt]] -> IO [Int]
evaluate program = execute $ BasicExt.convert $ UncheckedInsts.convert $ UncheckedInstsExt.convert program

mult :: Int -> Int -> [[UncheckedInstExt]]
mult n m =
  [ [ InsExtConst (Var 0) n,
      InsExtConst (Var 1) m,
      InsExtConst (Var 2) 1,
      InsExtConst (Var 3) 0,
      InsExtGoto (Lbl 1)
    ],
    [InsExtBranch (Var 0) (Lbl 2) Exit],
    [ InsExtCopyAdd (Var 1) [Var 3],
      InsExtCopySub (Var 2) [Var 0],
      InsExtGoto (Lbl 1)
    ]
  ]

testMult :: Int -> Int -> Int -> Test
testMult n m res = TestCase $ do
  let prog = mult n m
  mem <- evaluate prog
  (mem !! 6) @?= res

sumArray :: [Int] -> [[UncheckedInstExt]]
sumArray arr =
  let inits = concat $ zipWith initElem [0..] arr in
  [ inits ++ [
      InsExtConst (Var 0) 0,
      InsExtConst (Var 1) (length arr),
      InsExtGoto (Lbl 1)
    ],
    [ InsExtBranch (Var 1) (Lbl 2) Exit
    ],
    [
      InsExtConst (Var 2) 1,
      InsExtCopySub (Var 2) [Var 1],
      InsExtConst (ArrTargetVar 3) 0,
      InsExtArrayGet (Var 3) (Var 1),
      InsExtCopyAdd (ArrTargetVar 3) [Var 0],
      InsExtGoto (Lbl 1)
    ]
  ]
  where
    initElem :: Int -> Int -> [UncheckedInstExt]
    initElem idx val =
      [ InsExtConst (Var 1) idx,
        InsExtConst (ArrTargetVar 3) val,
        InsExtArraySet (Var 3) (Var 1)
      ]

testSumArray :: [Int] -> Int -> Test
testSumArray arr res = TestCase $ do
  let prog = sumArray arr
  mem <- evaluate prog
  (mem !! 3) @?= res

tests :: Test
tests =
  TestList
    [ testMult 11 11 121,
      testMult 0 42 0,
      testMult 42 0 0,
      testMult 14 1 14,
      testSumArray [1, 2, 3, 4, 5] 15,
      testSumArray [] 0,
      testSumArray [10] 10,
      testSumArray [10, 15, 33] 58
    ]
