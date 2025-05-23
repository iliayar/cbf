{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Executer where

import Basic
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, get, put, runStateT)
import qualified Data.Char
import Data.Default
import qualified Data.Vector as V
import Control.Monad (when)
import Text.Printf (printf)
import Data.List (intercalate)

cellSize :: Int
cellSize = 8
cellSizeMax :: Int
cellSizeMax = 2 ^ cellSize

newtype Pos = Pos Int

data Line a = Line
  { pos :: Pos,
    initElem :: a,
    left :: [a],
    right :: [a]
  }

instance (Default a) => Default (Line a) where
  def =
    Line
      { pos = Pos 0,
        initElem = def,
        left = [def],
        right = []
      }

moveLeft :: Line a -> Line a
moveLeft (Line {pos = Pos pos, left = e : es, right, initElem}) =
  Line
    { pos = Pos $ pos - 1,
      initElem,
      left = case es of
        [] -> [initElem]
        _ -> es,
      right = e : right
    }
moveLeft (Line {left = []}) = undefined

moveRight :: Line a -> Line a
moveRight (Line {pos = Pos pos, left, right = e : es, initElem}) =
  Line
    { pos = Pos $ pos + 1,
      left = e : left,
      right = es,
      initElem
    }
moveRight (Line {pos = Pos pos, left, right = [], initElem}) =
  Line
    { pos = Pos $ pos + 1,
      left = initElem : left,
      right = [],
      initElem
    }

current :: Line a -> a
current (Line {left = e : _}) = e
current (Line {left = []}) = undefined

setCurrent :: Line a -> a -> Line a
setCurrent line@(Line {left = _ : es}) e = line {left = e : es}
setCurrent (Line {left = []}) _ = undefined

extract :: Line a -> [a]
extract (Line {left, right}) = reverse left ++ right

newtype Addr = Addr Int
    deriving (Show)

data ExecuterD = ExecuterD
  { markers :: [Addr],
    pc :: Addr,
    insts :: V.Vector Brainfuck,
    line :: Line Int,
    output :: String,
    input :: String,
    mockIO :: Bool
  }

initExecuter :: Bool -> String -> [Brainfuck] -> ExecuterD
initExecuter mockIO inp insts =
  ExecuterD
    { markers = [],
      line = def,
      pc = Addr 0,
      insts = V.fromList insts,
      output = "",
      input = inp,
      mockIO
    }

type Executer' a = StateT ExecuterD IO a
newtype Executer a = Executer (Executer' a)
  deriving (Functor, Applicative, Monad, MonadIO)

modifyLine :: (Line Int -> Line Int) -> Executer ()
modifyLine f = Executer $ do
  executer <- get
  let executer' = executer {line = f (line executer)}
  put executer'

executerCurrent :: Executer Int
executerCurrent = Executer $ do
  executer <- get
  let res = current $ line executer
  return res

currentInst :: Executer (Maybe Brainfuck)
currentInst = Executer $ do
  ExecuterD {pc = (Addr pc), insts} <- get
  if V.length insts <= pc
    then return Nothing
    else return $ Just $ insts V.! pc

incPC :: Executer ()
incPC = Executer $ do
    executer@(ExecuterD { pc = (Addr pc) }) <- get
    put $ executer { pc = Addr $ pc + 1 }

getPC :: Executer Int
getPC = Executer $ do
    (ExecuterD { pc = (Addr pc) }) <- get
    return pc

pushMarker :: Executer ()
pushMarker = Executer $ do
    executer@(ExecuterD { pc, markers }) <- get
    put $ executer { markers = pc : markers }

popMarker :: Executer ()
popMarker = Executer $ do
    executer@(ExecuterD { markers }) <- get
    case markers of
        [] -> interrupt "Expected '[' in markers, but there is no, when poping"
        _ : ms -> put $ executer { markers = ms }

gotoLastMarker :: Executer ()
gotoLastMarker = Executer $ do
    executer@(ExecuterD { markers }) <- get
    case markers of
        [] -> interrupt "Expected '[' in markers, but there is no, when goto"
        m : ms -> put $ executer { pc = m, markers = m : ms }

interrupt :: String -> Executer' a
interrupt msg = do
    ExecuterD { pc = Addr pc } <- get
    fail $ "Malformed program(" ++ msg ++ ") at " ++ show pc

doOutput :: Int -> Executer ()
doOutput chCode = Executer $ do
  let ch = Data.Char.chr chCode
  st@(ExecuterD { output, mockIO }) <- get
  if mockIO then put $ st { output = output ++ [ch] }
  else liftIO $ putChar ch

doInput :: Executer Char
doInput = Executer $ do
   st@(ExecuterD { input, mockIO }) <- get
   if mockIO then do
     case input of
        ch' : is -> do
            put $ st { input = is }
            return ch'
        [] -> return '\0'
   else liftIO getChar
    

showLine :: [Int] -> String
showLine line =
    let idx = intercalate "|" $ fmap (printf "% 3d") [0..length line - 1] in
    let dat = intercalate "|" $ fmap (printf "%03d") line in
    "Idx: " ++ idx ++ "\n" ++ 
    "Mem: " ++ dat

debugState :: Executer ()
debugState = Executer $ do
    (ExecuterD { pc = (Addr pc), insts, markers, line }) <- get
    -- liftIO $ putStrLn $ "Executing instruction at " ++ show pc ++ ": " ++ show (insts V.! pc)
    -- liftIO $ putStrLn $ "Markers " ++ show markers
    liftIO $ putStrLn $ showLine (extract line)


executeMockIO :: String -> [Brainfuck] -> IO String
executeMockIO inp insts = snd <$> execute' True inp insts

execute :: [Brainfuck] -> IO [Int]
execute insts = fst <$> execute' False "" insts

execute' :: Bool -> String -> [Brainfuck] -> IO ([Int], String)
execute' mockIO inp insts = do
  (_, executer) <- case executeTillTheEnd of
    Executer stateT -> runStateT stateT (initExecuter mockIO inp insts)
  let mem = extract $ line executer
      out = output executer
  return (mem, out)
  where
    executeTillTheEnd :: Executer ()
    executeTillTheEnd = do
      inst <- currentInst
      case inst of
        Nothing -> return ()
        Just inst' -> do
            executeInst inst'
            incPC
            executeTillTheEnd

    executeInst :: Brainfuck -> Executer ()
    executeInst BfRead = do
      ch <- doInput
      let chCode = Data.Char.ord ch
      modifyLine (`setCurrent` chCode)
    executeInst BfWrite = do
      ch <- executerCurrent
      doOutput ch
      -- when (ch == 10) debugState
    executeInst BfMoveLeft = modifyLine moveLeft
    executeInst BfMoveRight = modifyLine moveRight
    executeInst BfInc = modifyLine $ \line ->
      let cur' = current line
       in setCurrent line $ if cur' == cellSizeMax - 1 then 0 else cur' + 1
    executeInst BfDec = modifyLine $ \line ->
      let cur' = current line
       in setCurrent line $ if cur' == 0 then cellSizeMax - 1 else cur' - 1
    executeInst BfLoopBegin = do
        ch <- executerCurrent
        if ch == 0 then gotoLoopEnd 0
        else pushMarker
    executeInst BfLoopEnd = do
        ch <- executerCurrent
        if ch == 0 then popMarker
        else gotoLastMarker

    gotoLoopEnd :: Int -> Executer ()
    gotoLoopEnd balance = do
        inst <- currentInst
        case inst of
            Nothing -> Executer $ interrupt "End of program, while ']' expected"
            Just BfLoopEnd -> if balance - 1 == 0 then return ()
                else do
                    incPC
                    gotoLoopEnd (balance - 1)
            Just BfLoopBegin -> do
                incPC
                gotoLoopEnd (balance + 1)
            Just _ -> do
                incPC
                gotoLoopEnd balance
