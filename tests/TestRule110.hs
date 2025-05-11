{-# LANGUAGE QuasiQuotes #-}

module TestRule110 where

import qualified BasicExt
import Executer (executeMockIO)
import qualified Imp
import Imp.Parser (parseProgramUnsafe)
import qualified SafeProc
import Test.HUnit
import qualified UncheckedInsts
import qualified UncheckedInstsExt
import qualified UncheckedProc
import NeatInterpolation (text)
import qualified Data.Text as T

evaluate :: Imp.Program -> IO String
evaluate program =
  executeMockIO "" $
    BasicExt.convert $
      UncheckedInsts.convert $
        UncheckedInstsExt.convert $
          UncheckedProc.convert $
            SafeProc.convert $
              Imp.convert program

rule110 :: Imp.Program
rule110 = parseProgramUnsafe $ T.unpack
  [text| 
    void print_array(int a[20]) {
        int SIZE = 20;
    
        int i = 0;
        while (SIZE - i) {
            if (a[i]) {
                write('#');
                write('#');
            } else {
                write(' ');
                write(' ');
            }
            i = i + 1;
        }
        write('\n');
    }
    
    int rule110(int c0, int c1, int c2) {
        if (c0) {
            if (c1) {
                if (c2) { return 0; } 
                else { return 1; }
            } else {
                if (c2) { return 1; }
                else { return 0; }
            }
        } else {
            if (c1) {
                if (c2) { return 1; }
                else { return 1; }
            } else {
                if (c2) { return 1; }
                else { return 0; }
            }
        }
    }
    
    int main() {
        int SIZE = 20;
        int cur[20];
        int next[20];
    
        int i = 0;
        while (SIZE - i) {
            cur[i] = 0;
            next[i] = 0;
            i = i + 1;
        }
    
        cur[SIZE - 10 + 0] = 0;
        cur[SIZE - 10 + 1] = 0;
        cur[SIZE - 10 + 2] = 0;
        cur[SIZE - 10 + 3] = 1;
        cur[SIZE - 10 + 4] = 1;
        cur[SIZE - 10 + 5] = 1;
        cur[SIZE - 10 + 6] = 0;
        cur[SIZE - 10 + 7] = 1;
        cur[SIZE - 10 + 8] = 1;
        cur[SIZE - 10 + 9] = 1;
        int iterations = 0;
    
        while (SIZE - iterations) {
            print_array(cur);
    
            i = 1;
            next[0] = rule110(0, cur[0], cur[1]);
            while (SIZE - 1 - i) {
                next[i] = rule110(cur[i - 1], cur[i], cur[i + 1]);
                i = i + 1;
            }
            next[SIZE - 1] = rule110(cur[SIZE - 2], cur[SIZE - 1], 0);
    
            i = 0;
            while (SIZE - i) {
                cur[i] = next[i];
                i = i + 1;
            }
            iterations = iterations + 1;
        }
    }
  |]

expected :: T.Text
expected =
  [text|
                          ######  ######
                        ####  ######  ##
                      ##########  ######
                    ####      ######  ##
                  ######    ####  ######
                ####  ##  ##########  ##
              ##############      ######
            ####          ##    ####  ##
          ######        ####  ##########
        ####  ##      ##########      ##
      ##########    ####      ##    ####
    ####      ##  ######    ####  ######
########    ########  ##  ##########  ##
####  ##  ####    ##########      ######
##############  ####      ##    ####  ##
            ########    ####  ##########
          ####    ##  ##########      ##
        ######  ########      ##    ####
      ####  ######    ##    ####  ######
    ##########  ##  ####  ##########  ##
  |]

testRule110 :: Test
testRule110 = TestCase $ do
  output <- evaluate rule110
  T.strip (T.pack output) @?= expected

tests :: Test
tests = TestList [ testRule110 ]
