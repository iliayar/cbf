{-# LANGUAGE NamedFieldPuns #-}

module ProgWriter where

import qualified Control.Monad.State as ST

data ProgWriterState = ProgWriterState
  { stIndent :: Int,
    stResult :: String
  }

type ProgWriter = ST.State ProgWriterState

indentIn :: ProgWriter ()
indentIn = do
  st@(ProgWriterState {stIndent}) <- ST.get
  ST.put $ st {stIndent = stIndent + 1}

indentOut :: ProgWriter ()
indentOut = do
  st@(ProgWriterState {stIndent}) <- ST.get
  ST.put $ st {stIndent = stIndent - 1}

write :: String -> ProgWriter ()
write s = do
  st@(ProgWriterState {stResult}) <- ST.get
  ST.put $ st {stResult = stResult ++ s}

indent :: ProgWriter ()
indent = do
  (ProgWriterState {stIndent}) <- ST.get
  write $ replicate (stIndent * 2) ' '

nl :: ProgWriter ()
nl = write "\n" >> indent

withIndent :: ProgWriter a -> ProgWriter a
withIndent inner = do
  indentIn
  r <- inner
  indentOut
  return r

runWriter :: ProgWriter a -> String
runWriter writer =
  let (_, st) = ST.runState writer $ ProgWriterState {stIndent = 0, stResult = ""}
  in stResult st
