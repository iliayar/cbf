module TestUncheckedProc where

import qualified BasicExt
import Executer (execute)
import Test.HUnit
import qualified UncheckedInsts
import qualified UncheckedInstsExt
import UncheckedProc (Func (..), Lbl (..), UncheckedProc (..), Var (..))
import qualified UncheckedProc as UP

evaluate :: [[[UncheckedProc]]] -> IO [Int]
evaluate program =
  execute $
    BasicExt.convert $
      UncheckedInsts.convert $
        UncheckedInstsExt.convert $
          UP.convert program

rv :: Int -> UP.Ref
rv s = UP.RefVar $ UP.Var s

mult :: Int -> Int -> [[[UncheckedProc]]]
mult n m =
  [ -- func 0
    [ -- #0:
      [ ProcConst (rv 0) n,
        ProcConst (rv 1) m,
        ProcConst (rv 2) 1,
        ProcConst (rv 3) 0,
        ProcGoto (Lbl 1),
        ProcGoto (Lbl 0) -- NOTE: Never called
      ],
      -- #1:
      [ ProcBranch (rv 0) (Lbl 2) Ret
      ],
      -- #2:
      [ ProcCopyAdd (rv 1) [rv 3],
        ProcCopySub (rv 2) [rv 0],
        ProcGoto (Lbl 1)
      ]
    ]
  ]

testMult :: Int -> Int -> Int -> Test
testMult n m res = TestCase $ do
  let prog = mult n m
  mem <- evaluate prog
  (mem !! 7) @?= res

pow :: Int -> Int -> [[[UncheckedProc]]]
pow n m =
  [ -- func 0
    [ -- #0:
      [ ProcConst (rv 10) n,
        ProcConst (rv 11) m,
        ProcCall (Func 2) 10,
        ProcConst (rv 0) 0,
        ProcCopyAdd (rv 12) [rv 0],
        ProcGoto Ret
      ]
    ],
    -- func 1 (mult) Var 2 = Var 0 * Var 1
    -- Var 0 -- input
    -- Var 1 -- input
    -- Var 2 -- output
    [ -- #0:
      [ ProcConst (rv 2) 1,
        ProcConst (rv 3) 0,
        ProcGoto (Lbl 1)
      ],
      -- #1:
      [ ProcBranch (rv 0) (Lbl 2) (Lbl 3)
      ],
      -- #2:
      [ ProcCopyAdd (rv 1) [rv 3],
        ProcCopySub (rv 2) [rv 0],
        ProcGoto (Lbl 1)
      ],
      -- #3:
      [ ProcConst (rv 2) 0,
        ProcCopyAdd (rv 3) [rv 2],
        ProcGoto Ret
      ]
    ],
    -- func 2 (pow) Var 2 = Var 0 ^ Var 1
    -- Var 0 -- input
    -- Var 1 -- input
    -- Var 2 -- output
    [ -- #0:
      [ ProcConst (rv 2) 1,
        ProcConst (rv 3) 1,
        ProcGoto (Lbl 1)
      ],
      -- #1:
      [ ProcBranch (rv 1) (Lbl 2) (Lbl 3)
      ],
      -- #2:
      [ ProcConst (rv 10) 0,
        ProcConst (rv 11) 0,
        ProcCopyAdd (rv 3) [rv 10],
        ProcCopyAdd (rv 0) [rv 11],
        ProcCall (Func 1) 10,
        ProcConst (rv 3) 0,
        ProcCopyAdd (rv 12) [rv 3],
        ProcCopySub (rv 2) [rv 1],
        ProcGoto (Lbl 1)
      ],
      -- #3:
      [ ProcConst (rv 2) 0,
        ProcCopyAdd (rv 3) [rv 2],
        ProcGoto Ret
      ]
    ]
  ]

testPow :: Int -> Int -> Int -> Test
testPow n m res = TestCase $ do
  let prog = pow n m
  mem <- evaluate prog
  (mem !! 4) @?= res

fact :: Int -> [[[UncheckedProc]]]
fact n =
  [ -- func 0
    [ -- #0:
      [ ProcConst (rv 10) n,
        ProcCall (Func 2) 10,
        ProcConst (rv 0) 0,
        ProcCopyAdd (rv 11) [rv 0],
        ProcGoto Ret
      ]
    ],
    -- func 1 (mult) Var 2 = Var 0 * Var 1
    -- Var 0 -- input
    -- Var 1 -- input
    -- Var 2 -- output
    [ -- #0:
      [ ProcConst (rv 2) 1,
        ProcConst (rv 3) 0
        -- ProcGoto (Lbl 1) -- NOTE: Should fallthrough
      ],
      -- #1:
      [ ProcBranch (rv 0) (Lbl 2) (Lbl 3)
      ],
      -- #2:
      [ ProcCopyAdd (rv 1) [rv 3],
        ProcCopySub (rv 2) [rv 0],
        ProcGoto (Lbl 1)
      ],
      -- #3:
      [ ProcConst (rv 2) 0,
        ProcCopyAdd (rv 3) [rv 2],
        ProcGoto Ret
      ]
    ],
    -- func 2 (factorial) Var 1 = (Var 0) !
    -- Var 0 -- input
    -- Var 1 -- output
    [ -- #0:
      [ ProcBranch (rv 0) (Lbl 2) (Lbl 1)
      ],
      -- #1:
      [ ProcConst (rv 1) 1,
        ProcGoto Ret
      ],
      -- #2:
      [ ProcConst (rv 10) 0,
        ProcCopyAdd (rv 0) [rv 10],
        ProcConst (rv 2) 1,
        ProcCopySub (rv 2) [rv 10],
        ProcCall (Func 2) 10,
        ProcConst (rv 3) 0,
        ProcCopyAdd (rv 11) [rv 3],
        ProcConst (rv 10) 0,
        ProcConst (rv 11) 0,
        ProcCopyAdd (rv 3) [rv 10],
        ProcCopyAdd (rv 0) [rv 11],
        ProcCall (Func 1) 10,
        ProcConst (rv 3) 0,
        ProcCopyAdd (rv 12) [rv 1],
        ProcGoto Ret
      ]
    ]
  ]

testFact :: Int -> Int -> Test
testFact n res = TestCase $ do
  let prog = fact n
  mem <- evaluate prog
  (mem !! 4) @?= res

sumArray :: [Int] -> [[[UncheckedProc]]]
sumArray arr =
  let inits = concat $ zipWith initElem [0..] arr in
  [ [ inits ++ [
        ProcConst (rv 0) 0,
        ProcConst (rv 1) (length arr),
        ProcGoto (Lbl 1)
      ],
      [ ProcBranch (rv 1) (Lbl 2) Exit
      ],
      [
        ProcConst (rv 2) 1,
        ProcCopySub (rv 2) [rv 1],
        ProcConst (UP.RefArrayValue (rv 3) 1) 0,
        ProcArrayGet (rv 3) (rv 1) 1,
        ProcCopyAdd (UP.RefArrayValue (rv 3) 1) [rv 0],
        ProcGoto (Lbl 1)
      ]
    ]
  ]
  where
    initElem :: Int -> Int -> [UncheckedProc]
    initElem idx val =
      [ ProcConst (rv 1) idx,
        ProcConst (UP.RefArrayValue (rv 3) 1) val,
        ProcArraySet (rv 3) (rv 1) 1
      ]

testSumArray :: [Int] -> Int -> Test
testSumArray arr res = TestCase $ do
  let prog = sumArray arr
  mem <- evaluate prog
  (mem !! 4) @?= res

tests :: Test
tests =
  TestList
    [ testMult 11 11 121,
      testMult 0 42 0,
      testMult 42 0 0,
      testMult 14 1 14,
      testPow 3 0 1,
      testPow 3 1 3,
      testPow 3 5 243,
      testPow 3 10 169,
      testPow 3 13 211,
      testPow 1 30 1,
      testPow 2 6 64,
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
