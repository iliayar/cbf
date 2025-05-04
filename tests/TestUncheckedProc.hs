module TestUncheckedProc where

import qualified BasicExt
import Executer (execute)
import Test.HUnit
import qualified UncheckedInsts
import qualified UncheckedInstsExt
import UncheckedProc (Func (..), Lbl (..), UncheckedProc (..), Var (..))
import qualified UncheckedProc

evaluate :: [[[UncheckedProc]]] -> IO [Int]
evaluate program =
  execute $
    BasicExt.convert $
      UncheckedInsts.convert $
        UncheckedInstsExt.convert $
          UncheckedProc.convert program

mult :: Int -> Int -> [[[UncheckedProc]]]
mult n m =
  [ -- func 0
    [ -- #0:
      [ ProcConst (Var 0) n,
        ProcConst (Var 1) m,
        ProcConst (Var 2) 1,
        ProcConst (Var 3) 0,
        ProcGoto (Lbl 1),
        ProcGoto (Lbl 0) -- NOTE: Never called
      ],
      -- #1:
      [ ProcBranch (Var 0) (Lbl 2) Ret
      ],
      -- #2:
      [ ProcCopyAdd (Var 1) [Var 3],
        ProcCopySub (Var 2) [Var 0],
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
      [ ProcConst (Var 10) n,
        ProcConst (Var 11) m,
        ProcCall (Func 2) 10,
        ProcConst (Var 0) 0,
        ProcCopyAdd (Var 12) [Var 0],
        ProcGoto Ret
      ]
    ],
    -- func 1 (mult) Var 2 = Var 0 * Var 1
    -- Var 0 -- input
    -- Var 1 -- input
    -- Var 2 -- output
    [ -- #0:
      [ ProcConst (Var 2) 1,
        ProcConst (Var 3) 0,
        ProcGoto (Lbl 1)
      ],
      -- #1:
      [ ProcBranch (Var 0) (Lbl 2) (Lbl 3)
      ],
      -- #2:
      [ ProcCopyAdd (Var 1) [Var 3],
        ProcCopySub (Var 2) [Var 0],
        ProcGoto (Lbl 1)
      ],
      -- #3:
      [ ProcConst (Var 2) 0,
        ProcCopyAdd (Var 3) [Var 2],
        ProcGoto Ret
      ]
    ],
    -- func 2 (pow) Var 2 = Var 0 ^ Var 1
    -- Var 0 -- input
    -- Var 1 -- input
    -- Var 2 -- output
    [ -- #0:
      [ ProcConst (Var 2) 1,
        ProcConst (Var 3) 1,
        ProcGoto (Lbl 1)
      ],
      -- #1:
      [ ProcBranch (Var 1) (Lbl 2) (Lbl 3)
      ],
      -- #2:
      [ ProcConst (Var 10) 0,
        ProcConst (Var 11) 0,
        ProcCopyAdd (Var 3) [Var 10],
        ProcCopyAdd (Var 0) [Var 11],
        ProcCall (Func 1) 10,
        ProcConst (Var 3) 0,
        ProcCopyAdd (Var 12) [Var 3],
        ProcCopySub (Var 2) [Var 1],
        ProcGoto (Lbl 1)
      ],
      -- #3:
      [ ProcConst (Var 2) 0,
        ProcCopyAdd (Var 3) [Var 2],
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
      [ ProcConst (Var 10) n,
        ProcCall (Func 2) 10,
        ProcConst (Var 0) 0,
        ProcCopyAdd (Var 11) [Var 0],
        ProcGoto Ret
      ]
    ],
    -- func 1 (mult) Var 2 = Var 0 * Var 1
    -- Var 0 -- input
    -- Var 1 -- input
    -- Var 2 -- output
    [ -- #0:
      [ ProcConst (Var 2) 1,
        ProcConst (Var 3) 0
        -- ProcGoto (Lbl 1) -- NOTE: Should fallthrough
      ],
      -- #1:
      [ ProcBranch (Var 0) (Lbl 2) (Lbl 3)
      ],
      -- #2:
      [ ProcCopyAdd (Var 1) [Var 3],
        ProcCopySub (Var 2) [Var 0],
        ProcGoto (Lbl 1)
      ],
      -- #3:
      [ ProcConst (Var 2) 0,
        ProcCopyAdd (Var 3) [Var 2],
        ProcGoto Ret
      ]
    ],
    -- func 2 (factorial) Var 1 = (Var 0) !
    -- Var 0 -- input
    -- Var 1 -- output
    [ -- #0:
      [ ProcBranch (Var 0) (Lbl 2) (Lbl 1)
      ],
      -- #1:
      [ ProcConst (Var 1) 1,
        ProcGoto Ret
      ],
      -- #2:
      [ ProcConst (Var 10) 0,
        ProcCopyAdd (Var 0) [Var 10],
        ProcConst (Var 2) 1,
        ProcCopySub (Var 2) [Var 10],
        ProcCall (Func 2) 10,
        ProcConst (Var 3) 0,
        ProcCopyAdd (Var 11) [Var 3],
        ProcConst (Var 10) 0,
        ProcConst (Var 11) 0,
        ProcCopyAdd (Var 3) [Var 10],
        ProcCopyAdd (Var 0) [Var 11],
        ProcCall (Func 1) 10,
        ProcConst (Var 3) 0,
        ProcCopyAdd (Var 12) [Var 1],
        ProcGoto Ret
      ]
    ]
  ]

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
      testFact 9 128
    ]
