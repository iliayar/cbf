{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Basic (Brainfuck, bfToString)
import qualified BasicExt
import Executer (execute)
import Imp
import Imp.Parser (parseProgram)
import qualified SafeProc
import System.Environment (getArgs)
import qualified UncheckedInsts
import qualified UncheckedInstsExt
import qualified UncheckedProc
import NeatInterpolation (text)
import qualified Data.Text as T

evaluate :: [Brainfuck] -> IO ()
evaluate prog = do
  mem <- execute prog
  putStrLn ""
  putStrLn $ "Memory: " ++ show mem

re :: String -> Expr
re s = ExprRef $ RefVar $ Var s

example :: String
example = T.unpack
  [text| 
    typedef int bool;
    typedef struct { bool a; int b[3][3]; } S;

    void foo(int c) {
        write(c);
        write(c + 1);
        write(c + 2);
    }

    void main() {
        write('E'); write('n'); write('t'); write('e'); write('r'); write(':'); write(' '); write('\n');
        foo(read() + 1);
    }
  |]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--help" : _ -> do
        putStrLn "Usafe: cbf [--help] [[-r] -c <FILE>]"
    _ -> return ()
  (args, doEval) <- case args of
    "-r" : as -> return (as, True)
    [] -> return ([], True)
    as -> return (as, False)
  (args, program) <- case args of
    [] -> do
      prog <- parseProgram "<example>" example
      return (["-r"], prog)
    "-c" : filename : as -> do
        content <- readFile filename
        prog <- parseProgram filename content
        return (as, prog)
    as -> return (as, Nothing)
  case program of
    Nothing -> return ()
    Just procImp -> do
      -- putStrLn "Imp:"
      -- putStrLn $ Imp.progToString procImp
      let procSafeProc = Imp.convert procImp
      -- putStrLn "SafeProc:"
      -- putStrLn $ SafeProc.progToString procSafeProc
      let procProc = SafeProc.convert procSafeProc
      -- putStrLn "UncheckedProc:"
      -- putStrLn $ UncheckedProc.progToString procProc
      let procInstsExt = UncheckedProc.convert procProc
      -- putStrLn "UncheckedInstExt:"
      -- putStrLn $ UncheckedInstsExt.progToString procInstsExt
      let procInsts = UncheckedInstsExt.convert procInstsExt
      -- putStrLn "UncheckedInst:"
      -- putStrLn $ UncheckedInsts.progToString procInsts
      let procBfExt = BasicExt.optimize $ UncheckedInsts.convert procInsts
      -- putStrLn "BasicExt:"
      -- putStrLn $ BasicExt.progToString procBfExt
      let procBf = BasicExt.convert procBfExt
      if doEval then evaluate procBf
      else putStrLn $ bfToString procBf
