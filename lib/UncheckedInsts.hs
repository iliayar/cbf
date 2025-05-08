module UncheckedInsts where

import qualified BasicExt as BE
import BasicExt (BrainfuckExt (..))
import Data.List (intercalate, intersperse)
import ProgWriter
import Control.Monad (forM_)

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
--
-- Also arrays: 
-- Layout: [ Header                               | Data          ]
--         [ Res    | Target | CurIdx | TargetIdx | a0 | a1 | ... ]
-- Init:   [ 0      | x      | 0      | i         | ...           ]
-- Iterate to target element:
--         [ a(j-1) | x      | j      | i         | aj | ...      ]
-- End of iteration: i = 0
-- When end iteration depending on operation (set/get) either copy `x` to aj or aj to `x`
-- Copying: On each forward pass cycle copy aj to N cells right or left
--
-- Array is identified by it's first cell (Res)
-- All Array operations exptect that Target and TargetIdx is properly settet up

newtype Var = Var Int

data InstCopyDir = DirLeft | DirRight
data UncheckedInst
  = InstConst Var Int
  | InstMoveAdd Var [Var]
  | InstMoveSub Var [Var]
  | InstWhile Var [UncheckedInst]
  | InstRead Var
  | InstWrite Var
  | InstIntrinsic [BrainfuckExt]
  | InstArrGet Var
  | InstArrSet Var
  | InstArrCopy Var Var

arraySize :: Int -> Int
arraySize n = 4 + n

arrayTargetVar :: Var -> Var
arrayTargetVar (Var i) = Var $ i + 1

arrayTargetIdxVar :: Var -> Var
arrayTargetIdxVar (Var i) = Var $ i + 3

convert :: [UncheckedInst] -> [BrainfuckExt]
convert = concatMap convert'
  where
    -- [ > [ - <4 + >4 ] < [ - > + < ] < [ - > + < ] >2 + > - ]
    arrayFwd :: [BrainfuckExt]
    arrayFwd =
      [ -- [ > [ - <4 + >4 ]
        BfExtLoopBegin,
        BfExtMoveRight 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft 4,
        BfExtInc 1,
        BfExtMoveRight 4,
        BfExtLoopEnd,

        -- < [ - > + < ]
        BfExtMoveLeft 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 1,
        BfExtInc 1,
        BfExtMoveLeft 1,
        BfExtLoopEnd,

        -- < [ - > + < ]
        BfExtMoveLeft 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 1,
        BfExtInc 1,
        BfExtMoveLeft 1,
        BfExtLoopEnd,

        -- < [ - > + < ]
        BfExtMoveLeft 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 1,
        BfExtInc 1,
        BfExtMoveLeft 1,
        BfExtLoopEnd,

        -- >2 + > -
        BfExtMoveRight 2,
        BfExtInc 1,
        BfExtMoveRight 1,
        BfExtDec 1,
        BfExtLoopEnd
      ]

    -- [ <3 [ - >4 + <4 ] >2 [ - < + > ] > [ - < + > ] < - ]
    arrayBck :: [BrainfuckExt]
    arrayBck =
      [ -- [ <3 [ - >4 + <4 ]
        BfExtLoopBegin,
        BfExtMoveLeft 3,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 4,
        BfExtInc 1,
        BfExtMoveLeft 4,
        BfExtLoopEnd,

        -- >2 [ - < + > ]
        BfExtMoveRight 2,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft 1,
        BfExtInc 1,
        BfExtMoveRight 1,
        BfExtLoopEnd,

        -- > [ - < + > ]
        BfExtMoveRight 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft 1,
        BfExtInc 1,
        BfExtMoveRight 1,
        BfExtLoopEnd,

        -- < - ]
        BfExtMoveLeft 1,
        BfExtDec 1,
        BfExtLoopEnd
      ]

    -- > [ - <3 + < + >4 ] <4 [ - >4 + <4 ] >2
    arrayGet :: [BrainfuckExt]
    arrayGet = 
        [ BfExtMoveRight 1,
          BfExtLoopBegin,
          BfExtDec 1,
          BfExtMoveLeft 3,
          BfExtInc 1,
          BfExtMoveLeft 1,
          BfExtInc 1,
          BfExtMoveRight 4,
          BfExtLoopEnd,
          BfExtMoveLeft 4,
          BfExtLoopBegin,
          BfExtDec 1,
          BfExtMoveRight 4,
          BfExtInc 1,
          BfExtMoveLeft 4,
          BfExtLoopEnd,
          BfExtMoveRight 2
        ]

    -- > [ - ] <3 [ - >3 + <3 ] >
    arraySet :: [BrainfuckExt]
    arraySet =
      [ BfExtMoveRight 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtLoopEnd,
        BfExtMoveLeft 3,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 3,
        BfExtInc 1,
        BfExtMoveLeft 3,
        BfExtLoopEnd,
        BfExtMoveRight 1
      ]

    -- [ > [ - <3 + < + >4 ] >N [ - ] <N <3 [ - >3 >N + <N <3 ] >2 [ - > + < ] < [ - > + < ] > + > - ]
    arrayCopyFwd :: InstCopyDir -> Int -> [BrainfuckExt]
    arrayCopyFwd dir n =
      [ -- [ > [ - <3 + < + >4 ]
        BfExtLoopBegin,
        BfExtMoveRight 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft 3,
        BfExtInc 1,
        BfExtMoveLeft 1,
        BfExtInc 1,
        BfExtMoveRight 4,
        BfExtLoopEnd,

        -- (>|<)N [ - ] (<|>)N <3 [ - >3 (>|<)N + (<|>)N <3 ]
        case dir of 
          DirRight -> BfExtMoveRight n
          DirLeft -> BfExtMoveLeft n,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtLoopEnd,
        case dir of 
          DirRight -> BfExtMoveLeft n
          DirLeft -> BfExtMoveRight n,
        BfExtMoveLeft 3,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 3,
        case dir of 
          DirRight -> BfExtMoveRight n
          DirLeft -> BfExtMoveLeft n,
        BfExtInc 1,
        case dir of 
          DirRight -> BfExtMoveLeft n
          DirLeft -> BfExtMoveRight n,
        BfExtMoveLeft 3,
        BfExtLoopEnd,
         
        -- >2 [ - > + < ]
        BfExtMoveRight 2,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 1,
        BfExtInc 1,
        BfExtMoveLeft 1,
        BfExtLoopEnd,

        -- < [ - > + < ]
        BfExtMoveLeft 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight 1,
        BfExtInc 1,
        BfExtMoveLeft 1,
        BfExtLoopEnd,

        --  > + > - ]
        BfExtMoveRight 1,
        BfExtInc 1,
        BfExtMoveRight 1,
        BfExtDec 1,
        BfExtLoopEnd
      ]

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
      -- NOTE: Sorting js may optimize count of instructions
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
    convert' (InstArrGet (Var i)) =
        [ -- >i >3
          BfExtMoveRight i,
          BfExtMoveRight 3
        ] ++
        arrayFwd ++ 
        arrayGet ++ 
        arrayBck ++ 
        [ -- <2 <i
          BfExtMoveLeft 2,
          BfExtMoveLeft i
        ]
    convert' (InstArrSet (Var i)) =
        [ -- >i >3
          BfExtMoveRight i,
          BfExtMoveRight 3
        ] ++
        arrayFwd ++
        arraySet ++ 
        arrayBck ++ 
        [ -- <2 <i
          BfExtMoveLeft 2,
          BfExtMoveLeft i
        ]
    convert' (InstArrCopy (Var i) (Var j)) =
        let (dir, n) = if j > i then (DirRight, j - i) else (DirLeft, i - j) in
        [ -- >i >3
          BfExtMoveRight i,
          BfExtMoveRight 3
        ] ++
        arrayCopyFwd dir n ++ 
        [ BfExtMoveLeft 1 ] ++
        arrayBck ++ 
        [ -- <2 <i
          BfExtMoveLeft 2,
          BfExtMoveLeft i
        ]

progToString :: [UncheckedInst] -> String
progToString = runWriter . progToString'

progToString' :: [UncheckedInst] -> ProgWriter ()
progToString' = instsToString
    where
        instsToString :: [UncheckedInst] -> ProgWriter ()
        instsToString block = forM_ (intersperse nl $ fmap instToString block) id

        instToString :: UncheckedInst -> ProgWriter ()
        instToString (InstConst var n) = write $ varToString var ++ " := " ++ show n
        instToString (InstMoveAdd v vs) = write $
            intercalate "; " (fmap (\v' -> varToString v' ++ " += " ++ varToString v) vs) ++
            "; " ++ varToString v ++ " := 0"
        instToString (InstMoveSub v vs) = write $
            intercalate "; " (fmap (\v' -> varToString v' ++ " -= " ++ varToString v) vs) ++
            "; " ++ varToString v ++ " := 0"
        instToString (InstWhile v body) = do
            write $ "while " ++ varToString v ++ " != 0 {"
            withIndent $ nl >> instsToString  body
            nl >> write "}"
        instToString (InstRead v) = write $ "read " ++ varToString v
        instToString (InstWrite v) = write $ "write " ++ varToString v
        instToString (InstIntrinsic bf) = write $ "intrinsic " ++ BE.progToString bf
        instToString (InstArrGet v) = write $ "get " ++ varToString v
        instToString (InstArrSet v) = write $ "set " ++ varToString v
        instToString (InstArrCopy f t) = write $ varToString t ++ "[] = " ++ varToString f ++ "[]"

        varToString (Var i) = "%" ++ show i
