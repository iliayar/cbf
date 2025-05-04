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

tests :: Test
tests =
  TestList
    [ testMult 11 11 121,
      testMult 0 42 0,
      testMult 42 0 0,
      testMult 14 1 14
    ]
