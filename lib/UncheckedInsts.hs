module UncheckedInsts where

import qualified BasicExt as BE
import BasicExt (BrainfuckExt (..))
import Data.List (intercalate)

-- This is one uses more familiar procedural "instructions". It is however not an SSA
-- const(n) = [ - ] +n
-- %i = n -- const(%i, n) -- >i const(n) <i
--
-- %j1 := %j1 + %i; %j2 = %j2 + %i; ...; %i = 0 -- move_add(%i, %j1, %j2, ...) -- >i [ - <i >j1 + <j1 ( >j2 + <j2 ) ...  >i ] <i
-- %j1 := %j1 - %i; %j2 = %j2 - %i; ...; %i = 0 -- move_sub(%i, %j1, %j2, ...) -- >i [ - <i >j1 - <j1 ( >j2 - <j2 ) ...  >i ] <i
--
-- while (%i != 0) #j --
--   >i [ <i #j >i ] <i
--
-- Variables are unchecked
--
-- How to make if:
-- if (%i != 0) #j #k; %i = 0 --
--   %iz := 0
--   while (%i != 0) {
--      #j
--      %iz = 1 
--      %i = 0
--   }
--   while (%iz != 0) {
--      #k
--      %iz = 0
--   }

newtype Var = Var Int

data UncheckedInst
  = InstConst Var Int
  | InstMoveAdd Var [Var]
  | InstMoveSub Var [Var]
  | InstWhile Var [UncheckedInst]
  | InstRead Var
  | InstWrite Var
  | InstIntrinsic [BrainfuckExt]

convert :: [UncheckedInst] -> [BrainfuckExt]
convert = concatMap convert'
  where
    convert' :: UncheckedInst -> [BrainfuckExt]
    convert' (InstConst (Var i) n) =
      [ -- >i
        BfExtMoveRight i,
        -- [ - ] +n
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtLoopEnd,
        BfExtInc n,
        -- <i
        BfExtMoveLeft i
      ]
    convert' (InstMoveAdd (Var i) js) =
      [ -- >i
        BfExtMoveRight i,
        -- [ - <i
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft i
      ] ++
      -- >j + <j
      -- >j + <j
      
      -- >j + <j
      concatMap (\(Var j) ->
        [ BfExtMoveRight j,
          BfExtInc 1,
          BfExtMoveLeft j
        ]) js ++
       [ BfExtMoveRight i,
         BfExtLoopEnd,
         BfExtMoveLeft i
       ]
    convert' (InstMoveSub (Var i) js) =
      [ -- >i
        BfExtMoveRight i,
        -- [ - <i
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft i
      ] ++
      -- >j + <j
      -- >j + <j
      
      -- >j + <j
      concatMap (\(Var j) ->
        [ BfExtMoveRight j,
          BfExtDec 1,
          BfExtMoveLeft j
        ]) js ++
       [ BfExtMoveRight i,
         BfExtLoopEnd,
         BfExtMoveLeft i
       ]
    convert' (InstWhile (Var i) body) =
      [ -- >i [ <i
        BfExtMoveRight i,
        BfExtLoopBegin,
        BfExtMoveLeft i
      ] ++
      -- #j
      -- #j
      
      -- #j
      convert body ++
      [ -- >i ] <i
        BfExtMoveRight i,
        BfExtLoopEnd,
        BfExtMoveLeft i
      ]
    convert' (InstRead (Var i)) =
      [ -- >i , <i
        BfExtMoveRight i,
        BfExtRead,
        BfExtMoveLeft i
      ]
    convert' (InstWrite (Var i)) =
      [ -- >i . <i
        BfExtMoveRight i,
        BfExtWrite,
        BfExtMoveLeft i
      ]
    convert' (InstIntrinsic bf) = bf

progToString :: [UncheckedInst] -> String
progToString = progToStringWithIndent 0

progToStringWithIndent :: Int -> [UncheckedInst] -> String
progToStringWithIndent = instsToString
    where
        getIndent :: Int -> String
        getIndent indent = replicate (indent * 2) ' '

        instsToString :: Int -> [UncheckedInst] -> String
        instsToString indent block = intercalate "\n" $ fmap (instToString indent) block

        instToString :: Int -> UncheckedInst -> String
        instToString indent (InstConst var n) = getIndent indent ++ varToString var ++ " := " ++ show n
        instToString indent (InstMoveAdd v vs) = getIndent indent ++
            intercalate "; " (fmap (\v' -> varToString v' ++ " += " ++ varToString v) vs) ++
            "; " ++ varToString v ++ " := 0"
        instToString indent (InstMoveSub v vs) = getIndent indent ++
            intercalate "; " (fmap (\v' -> varToString v' ++ " -= " ++ varToString v) vs) ++
            "; " ++ varToString v ++ " := 0"
        instToString indent (InstWhile v body) = getIndent indent ++ "while " ++ varToString v ++ " != 0 {\n" ++
            instsToString (indent + 1) body ++ "\n" ++ getIndent indent ++ "}"
        instToString indent (InstRead v) = getIndent indent ++ "read " ++ varToString v
        instToString indent (InstWrite v) = getIndent indent ++ "write " ++ varToString v
        instToString indent (InstIntrinsic bf) = getIndent indent ++ "intrinsic " ++ BE.progToString bf

        varToString (Var i) = "%" ++ show i
