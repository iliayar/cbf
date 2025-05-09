module TestUncheckedInsts where

import qualified BasicExt
import Executer (execute)
import Test.HUnit
import UncheckedInsts
import qualified UncheckedInsts

evaluate :: [UncheckedInst] -> IO [Int]
evaluate program = execute $ BasicExt.convert $ UncheckedInsts.convert program

mult :: Int -> Int -> [UncheckedInst]
mult n m =
  [ InstConst (Var 1000) 0,
    InstConst (Var 0) n,
    InstConst (Var 1) m,
    InstMoveAdd (Var 0) [Var 2, Var 1000],
    InstMoveAdd (Var 1000) [Var 0],
    InstConst (Var 3) 0,
    InstWhile
      (Var 0)
      [ InstMoveAdd (Var 1) [Var 4, Var 1000],
        InstMoveAdd (Var 1000) [Var 1],
        InstMoveAdd (Var 4) [Var 3],
        InstConst (Var 5) 1,
        InstMoveSub (Var 5) [Var 0]
      ],
    InstWrite (Var 3)
  ]

testMult :: Int -> Int -> Int -> Test
testMult n m res = TestCase $ do
  let prog = mult n m
  mem <- evaluate prog
  (mem !! 3) @?= res

array :: [UncheckedInst]
array =
  [ -- %10[0] = 10
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) 1],
    InstConst (Var 0) 10,
    InstMoveAdd (Var 0) [arrayTargetVar (Var 10) 1],
    InstArrSet (Var 10) 1,
    -- %10[5] = 5
    InstConst (Var 0) 5,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) 1],
    InstConst (Var 0) 5,
    InstMoveAdd (Var 0) [arrayTargetVar (Var 10) 1],
    InstArrSet (Var 10) 1,
    -- %10[9] = 1
    InstConst (Var 0) 9,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) 1],
    InstConst (Var 0) 1,
    InstMoveAdd (Var 0) [arrayTargetVar (Var 10) 1],
    InstArrSet (Var 10) 1,
    -- %30[5] = 55
    InstConst (Var 0) 5,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) 1],
    InstConst (Var 0) 55,
    InstMoveAdd (Var 0) [arrayTargetVar (Var 30) 1],
    InstArrSet (Var 30) 1,
    -- %1 = %30[5]
    InstConst (Var 0) 5,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) 1],
    InstConst (arrayTargetVar (Var 30) 1) 0,
    InstArrGet (Var 30) 1,
    InstMoveAdd (arrayTargetVar (Var 30) 1) [Var 1],
    -- %30[] = %10[]
    InstConst (Var 0) 10,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) 1],
    InstConst (arrayTargetVar (Var 10) 1) 0,
    InstArrCopy (Var 10) (Var 30) 1,
    -- %2 = %30[5]
    InstConst (Var 0) 5,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) 1],
    InstConst (arrayTargetVar (Var 30) 1) 0,
    InstArrGet (Var 30) 1,
    InstMoveAdd (arrayTargetVar (Var 30) 1) [Var 2],
    -- %3 = %30[0]
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) 1],
    InstConst (arrayTargetVar (Var 30) 1) 0,
    InstArrGet (Var 30) 1,
    InstMoveAdd (arrayTargetVar (Var 30) 1) [Var 3],
    -- %4 = %10[9]
    InstConst (Var 0) 9,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) 1],
    InstConst (arrayTargetVar (Var 10) 1) 0,
    InstArrGet (Var 10) 1,
    InstMoveAdd (arrayTargetVar (Var 10) 1) [Var 4],
    -- %30[9] = 11
    InstConst (Var 0) 9,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) 1],
    InstConst (Var 0) 11,
    InstMoveAdd (Var 0) [arrayTargetVar (Var 30) 1],
    InstArrSet (Var 30) 1,
    -- %10[] = %30[]
    InstConst (Var 0) 10,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) 1],
    InstConst (arrayTargetVar (Var 30) 1) 0,
    InstArrCopy (Var 30) (Var 10) 1,
    -- %5 = %10[9]
    InstConst (Var 0) 9,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) 1],
    InstConst (arrayTargetVar (Var 10) 1) 0,
    InstArrGet (Var 10) 1,
    InstMoveAdd (arrayTargetVar (Var 10) 1) [Var 5]
  ]

testArray :: Test
testArray = TestCase $ do
  mem <- evaluate array
  (mem !! 1) @?= 55
  (mem !! 2) @?= 5
  (mem !! 3) @?= 10
  (mem !! 4) @?= 1
  (mem !! 5) @?= 11

array2 :: [UncheckedInst]
array2 =
  let s = 2 in
  [ -- %10[0] = 10, 100
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (Var 0) 10,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 0],
    InstConst (Var 0) 100,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 1],
    InstArrSet (Var 10) s,
    -- %30[] = %10[]
    InstConst (Var 0) 3,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrCopy (Var 10) (Var 30) s,
    -- %1, %2 = %10[0]
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrGet (Var 10) s,
    InstMoveAdd (arrayTargetVar' (Var 10) s 0) [Var 1],
    InstMoveAdd (arrayTargetVar' (Var 10) s 1) [Var 2],
    -- %30[2] = 5, 50
    InstConst (Var 0) 2,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) s],
    InstConst (Var 0) 5,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 30) s 0],
    InstConst (Var 0) 50,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 30) s 1],
    InstArrSet (Var 30) s,
    -- %10[] = %30[]
    InstConst (Var 0) 3,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 30) s],
    InstConst (arrayTargetVar' (Var 30) s 0) 0,
    InstConst (arrayTargetVar' (Var 30) s 1) 0,
    InstArrCopy (Var 30) (Var 10) s,
    -- %3, %4 = %10[2]
    InstConst (Var 0) 2,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrGet (Var 10) s,
    InstMoveAdd (arrayTargetVar' (Var 10) s 0) [Var 3],
    InstMoveAdd (arrayTargetVar' (Var 10) s 1) [Var 4],
    -- %5, %6 = %10[0]
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrGet (Var 10) s,
    InstMoveAdd (arrayTargetVar' (Var 10) s 0) [Var 5],
    InstMoveAdd (arrayTargetVar' (Var 10) s 1) [Var 6]
  ]

testArray2 :: Test
testArray2 = TestCase $ do
  mem <- evaluate array2
  (mem !! 1) @?= 10
  (mem !! 2) @?= 100
  (mem !! 3) @?= 5
  (mem !! 4) @?= 50
  (mem !! 5) @?= 10
  (mem !! 6) @?= 100

array3 :: [UncheckedInst]
array3 =
  let s = 2 in
  [ -- %10[0] = 10, 100
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (Var 0) 10,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 0],
    InstConst (Var 0) 100,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 1],
    InstArrSet (Var 10) s,
    -- %1, %2 = %10[0]
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrGet (Var 10) s,
    InstMoveAdd (arrayTargetVar' (Var 10) s 0) [Var 1],
    InstMoveAdd (arrayTargetVar' (Var 10) s 1) [Var 2],
    -- %10[2] = 5, 50
    InstConst (Var 0) 2,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (Var 0) 5,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 0],
    InstConst (Var 0) 50,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 1],
    InstArrSet (Var 10) s,
    -- %3, %4 = %10[2]
    InstConst (Var 0) 2,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrGet (Var 10) s,
    InstMoveAdd (arrayTargetVar' (Var 10) s 0) [Var 3],
    InstMoveAdd (arrayTargetVar' (Var 10) s 1) [Var 4],
    -- %5, %6 = %10[0]
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrGet (Var 10) s,
    InstMoveAdd (arrayTargetVar' (Var 10) s 0) [Var 5],
    InstMoveAdd (arrayTargetVar' (Var 10) s 1) [Var 6],
    -- %10[0] = 20, 200
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (Var 0) 20,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 0],
    InstConst (Var 0) 200,
    InstMoveAdd (Var 0) [arrayTargetVar' (Var 10) s 1],
    InstArrSet (Var 10) s,
    -- %7, %8 = %10[0]
    InstConst (Var 0) 0,
    InstMoveAdd (Var 0) [arrayTargetIdxVar (Var 10) s],
    InstConst (arrayTargetVar' (Var 10) s 0) 0,
    InstConst (arrayTargetVar' (Var 10) s 1) 0,
    InstArrGet (Var 10) s,
    InstMoveAdd (arrayTargetVar' (Var 10) s 0) [Var 7],
    InstMoveAdd (arrayTargetVar' (Var 10) s 1) [Var 8]
  ]

testArray3 :: Test
testArray3 = TestCase $ do
  mem <- evaluate array3
  (mem !! 1) @?= 10
  (mem !! 2) @?= 100
  (mem !! 3) @?= 5
  (mem !! 4) @?= 50
  (mem !! 5) @?= 10
  (mem !! 6) @?= 100
  (mem !! 7) @?= 20
  (mem !! 8) @?= 200

tests :: Test
tests =
  TestList
    [ testMult 11 11 121,
      testMult 0 42 0,
      testMult 42 0 0,
      testMult 14 1 14,
      testArray,
      testArray2,
      testArray3
    ]
