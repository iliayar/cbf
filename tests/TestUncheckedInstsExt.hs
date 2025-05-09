module TestUncheckedInstsExt where

import qualified BasicExt
import Executer (execute)
import Test.HUnit
import qualified UncheckedInsts
import UncheckedInstsExt (Lbl (..), UncheckedInstExt (..), Var (..))
import qualified UncheckedInstsExt as UIE

evaluate :: [[UncheckedInstExt]] -> IO [Int]
evaluate program = execute $ BasicExt.convert $ UncheckedInsts.convert $ UIE.convert program

rv :: Int -> UIE.Ref
rv n = UIE.RefVar $ Var n

mult :: Int -> Int -> [[UncheckedInstExt]]
mult n m =
  [ [ InsExtConst (rv 0) n,
      InsExtConst (rv 1) m,
      InsExtConst (rv 2) 1,
      InsExtConst (rv 3) 0,
      InsExtGoto (Lbl 1)
    ],
    [InsExtBranch (rv 0) (Lbl 2) Exit],
    [ InsExtCopyAdd (rv 1) [rv 3],
      InsExtCopySub (rv 2) [rv 0],
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
      InsExtConst (rv 0) 0,
      InsExtConst (rv 1) (length arr),
      InsExtGoto (Lbl 1)
    ],
    [ InsExtBranch (rv 1) (Lbl 2) Exit
    ],
    [
      InsExtConst (rv 2) 1,
      InsExtCopySub (rv 2) [rv 1],
      InsExtConst (UIE.RefArrayValue (rv 3) 1) 0,
      InsExtArrayGet (rv 3) (rv 1) 1,
      InsExtCopyAdd (UIE.RefArrayValue (rv 3) 1) [rv 0],
      InsExtGoto (Lbl 1)
    ]
  ]
  where
    initElem :: Int -> Int -> [UncheckedInstExt]
    initElem idx val =
      [ InsExtConst (rv 1) idx,
        InsExtConst (UIE.RefArrayValue (rv 3) 1) val,
        InsExtArraySet (rv 3) (rv 1) 1
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
