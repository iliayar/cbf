module Main where

import Test.HUnit
import qualified System.Exit as Exit

import qualified TestUncheckedInsts
import qualified TestUncheckedInstsExt
import qualified TestUncheckedProc
import qualified TestSafeProc
import qualified TestImp
import qualified TestRule110
import qualified TestFactorial


tests :: Test
tests = TestList 
  [ TestLabel "TestUncheckedInsts" TestUncheckedInsts.tests
  , TestLabel "TestUncheckedInstsExt" TestUncheckedInstsExt.tests
  , TestLabel "TestUncheckedProc" TestUncheckedProc.tests
  , TestLabel "TestSafeProc" TestSafeProc.tests
  , TestLabel "TestImp" TestImp.tests
  , TestLabel "TestRule110" TestRule110.tests
  , TestLabel "TestFactorial" TestFactorial.tests
  ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
