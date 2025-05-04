module TestUncheckedInsts where

import Test.HUnit
import UncheckedInsts (UncheckedInst(..), Var(..))

import qualified UncheckedInsts
import qualified BasicExt

import Executer (execute)

evaluate :: [UncheckedInst] -> IO [Int]
evaluate program = execute $ BasicExt.convert $ UncheckedInsts.convert program

mult :: Int -> Int -> [UncheckedInst]
mult n m =
  [
    InstConst (Var 1000) 0,
    InstConst (Var 0) n,
    InstConst (Var 1) m,

    InstMoveAdd (Var 0) [Var 2, Var 1000],
    InstMoveAdd (Var 1000) [Var 0],

    InstConst (Var 3) 0,
    InstWhile (Var 0) [
        InstMoveAdd (Var 1) [Var 4, Var 1000],
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

tests :: Test
tests = TestList 
  [ testMult 11 11 121
  , testMult 0 42 0
  , testMult 42 0 0
  , testMult 14 1 14
  ]
