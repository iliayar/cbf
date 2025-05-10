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
  | InstArrGet Var Int
  | InstArrSet Var Int
  | InstArrCopy Var Var Int

arraySize :: Int -> Int -> Int
arraySize n s = 2 * s + 2 + n * s

arrayTargetVar :: Var -> Int -> Var
arrayTargetVar v s = arrayTargetVar' v s 0

arrayTargetVar' :: Var -> Int -> Int -> Var
arrayTargetVar' (Var i) s k = Var $ i + s + k

arrayTmpVar' :: Var -> Int -> Int -> Var
arrayTmpVar' (Var i) _ k = Var $ i + k

arrayTmpIndexVar' :: Var -> Int -> Var
arrayTmpIndexVar' (Var i) s = Var $ i + 2 * s

arrayTargetIdxVar :: Var -> Int -> Var
arrayTargetIdxVar (Var i) s = Var $ i + 2*s + 1

structOffset :: Var -> Int -> Var
structOffset (Var i) o = Var $ i + o

convert :: [UncheckedInst] -> [BrainfuckExt]
convert = concatMap convert'
  where
    -- [ 
    --   {>k [ - <(k + 1 + 2*s) >(k - 1) + <(k - 1) >(2*s + 1 + k) ] <k}(k=1..s)
    --   [ - >s + <s ] < [ - >s + <s ] 
    --   <s { >(k - 1) [ - >s + <s ] <(k - 1) }(k=1..s)
    --   >(2*s) + > -
    -- ]
    arrayFwd :: Int -> [BrainfuckExt]
    arrayFwd s =
      -- [
      [BfExtLoopBegin] ++
      -- {>k [ - <(k + 1 + 2*s) >(k - 1) + <(k - 1) >(2*s + 1 + k) ] <k}(k=1..s)
      concatMap (\k -> [
        BfExtMoveRight k,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft $ k + 1 + 2*s,
        BfExtMoveRight $ k  - 1,
        BfExtInc 1,
        BfExtMoveLeft $ k - 1,
        BfExtMoveRight $ 2*s + 1 + k,
        BfExtLoopEnd,
        BfExtMoveLeft k
      ]) [1..s] ++

      [ -- [ - >s + <s ] < [ - >s + <s ]
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight s,
        BfExtInc 1,
        BfExtMoveLeft s,
        BfExtLoopEnd,
        BfExtMoveLeft 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight s,
        BfExtInc 1,
        BfExtMoveLeft s,
        BfExtLoopEnd
      ] ++
      -- <s { >(k - 1) [ - >s + <s ] <(k - 1) }(k=1..s)
      [ BfExtMoveLeft s ] ++
      concatMap (\k -> [ 
        BfExtMoveRight $ k - 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight s,
        BfExtInc 1,
        BfExtMoveLeft s,
        BfExtLoopEnd,
        BfExtMoveLeft $ k - 1
      ]) [1..s] ++
      [ -- >(2*s) + > - ]
        BfExtMoveRight $ 2 * s,
        BfExtInc 1,
        BfExtMoveRight 1,
        BfExtDec 1,
        BfExtLoopEnd
      ]

    -- [
    --   {<k [ - <s + >s ] >k}(k=1..s)
    --   [ - <s + >s ] <s
    --   <2s { [ - >(2s + 2) + <(2s + 2) ] >){_=1..s}
    --   >s -
    -- ]
    arrayBck :: Int -> [BrainfuckExt]
    arrayBck s =
      [ BfExtLoopBegin ] ++
      -- {<k [ - <s + >s ] >k}(k=1..s)
      concatMap (\k -> [
        BfExtMoveLeft k,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft s,
        BfExtInc 1,
        BfExtMoveRight s,
        BfExtLoopEnd,
        BfExtMoveRight k
      ]) [1..s] ++
      [ -- [ - <s + >s ] <s <2s
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft s,
        BfExtInc 1,
        BfExtMoveRight s,
        BfExtLoopEnd,
        BfExtMoveLeft $ 3*s
      ] ++
      -- { [ - >(2s + 2) + <(2s + 2) ] >){_=1..s}
      concatMap (const [
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight $ 2*s + 2,
        BfExtInc 1,
        BfExtMoveLeft $ 2*s + 2,
        BfExtLoopEnd,
        BfExtMoveRight 1
      ]) [1..s] ++
      [ -- >s - ]
        BfExtMoveRight s,
        BfExtDec 1,
        BfExtLoopEnd
      ]

    -- {>k [ - <(k + 1 + s) >(k - 1) + <s + >s <(k - 1) >(s + 1 + k) ] <k}(k=1..s)
    -- {<(1 + 2s) >(k - 1) [ - <(k - 1) >(2*s + 1 + k) + <(k + 1 + 2*s) >(k - 1) ] <(k - 1) >(2*s + 1)}(k=1..s)
    -- <
    arrayGet :: Int -> [BrainfuckExt]
    arrayGet s = 
        concatMap (\k -> [
          BfExtMoveRight k,
          BfExtLoopBegin,
          BfExtDec 1,
          BfExtMoveLeft $ k + 1 + s,
          BfExtMoveRight $ k - 1,
          BfExtInc 1,
          BfExtMoveLeft s,
          BfExtInc 1,
          BfExtMoveRight s,
          BfExtMoveLeft $ k - 1,
          BfExtMoveRight $ s + 1 + k,
          BfExtLoopEnd,
          BfExtMoveLeft k
        ]) [1..s] ++
        concatMap (\k -> [
          BfExtMoveLeft $ 1 + 2*s,
          BfExtMoveRight $ k - 1,
          BfExtLoopBegin,
          BfExtDec 1,
          BfExtMoveLeft $ k - 1,
          BfExtMoveRight $ 2*s + 1 + k,
          BfExtInc 1,
          BfExtMoveLeft $ k + 1 + 2*s,
          BfExtMoveRight $ k - 1,
          BfExtLoopEnd,
          BfExtMoveLeft $ k - 1,
          BfExtMoveRight $ 2*s + 1
        ]) [1..s] ++
        [ BfExtMoveLeft 1 ]

    -- {> [ - ]}(_=1..s) <(s + 2)
    -- {[ - >(s + 2) + <(s + 2) ] <}(_=1..s)
    -- >(s + 1)
    arraySet :: Int -> [BrainfuckExt]
    arraySet s =
      concatMap (const [
        BfExtMoveRight 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtLoopEnd
      ]) [1..s] ++
      [ BfExtMoveLeft $ s + 2 ] ++
      concatMap (const [
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight $ s + 2,
        BfExtInc 1,
        BfExtMoveLeft $ s + 2,
        BfExtLoopEnd,
        BfExtMoveLeft 1
      ]) [1..s] ++
      [ BfExtMoveRight $ s + 1 ]

    -- [ 
    --   > {[ - <(s + 2) + <s + >(2s + 2) ] >}(_=1..s) <(s + 1)
    --   > (>|<)N {[ - ] >}(_=1..s) <s (<|>)N <
    --   < {<k [ - >(k + 2) (>|<)N >(s - k) + <(s - k) (<|>)N <(2 + k) ] >k}(k=1..s) >
    --   [ - >s + <s ] < [ - >s + <s ] >s + > -
    -- ]
    arrayCopyFwd :: InstCopyDir -> Int -> Int -> [BrainfuckExt]
    arrayCopyFwd dir n s =
      [ -- [ >
        BfExtLoopBegin,
        BfExtMoveRight 1
      ] ++
      -- {[ - <(s + 2) + <s + >(2s + 2) ] >}(_=1..s)
      concatMap (const [ 
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveLeft $ s + 2,
        BfExtInc 1,
        BfExtMoveLeft s,
        BfExtInc 1,
        BfExtMoveRight $ 2*s + 2,
        BfExtLoopEnd,
        BfExtMoveRight 1
      ]) [1..s] ++
      [ -- <(s + 1) > (>|<)N 
        BfExtMoveLeft $ s + 1,
        BfExtMoveRight 1,
        case dir of
          DirRight -> BfExtMoveRight n
          DirLeft -> BfExtMoveLeft n
      ] ++
      -- {[ - ] >}(_=1..s) 
      concatMap (const [
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtLoopEnd,
        BfExtMoveRight 1
      ]) [1..s] ++
      [ -- <s (<|>)N < < 
        BfExtMoveLeft s,
        case dir of
          DirRight -> BfExtMoveLeft n
          DirLeft -> BfExtMoveRight n,
        BfExtMoveLeft 2
      ] ++
      -- {<k [ - >(k + 2) (>|<)N >(s - k) + <(s - k)  + (<|>)N <(2 + k) ] >k}(k=1..s)
      concatMap (\k -> [
        BfExtMoveLeft k,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight $ k + 2,
        case dir of
          DirRight -> BfExtMoveRight n
          DirLeft -> BfExtMoveLeft n,
        BfExtMoveRight $ s - k,
        BfExtInc 1,
        BfExtMoveLeft $ s - k,
        case dir of
          DirRight -> BfExtMoveLeft n
          DirLeft -> BfExtMoveRight n,
        BfExtMoveLeft $ 2 + k,
        BfExtLoopEnd,
        BfExtMoveRight k
      ]) [1..s] ++
      [ -- > [ - >s + <s ]
        BfExtMoveRight 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight s,
        BfExtInc 1,
        BfExtMoveLeft s,
        BfExtLoopEnd,

        -- < [ - >s + <s ]
        BfExtMoveLeft 1,
        BfExtLoopBegin,
        BfExtDec 1,
        BfExtMoveRight s,
        BfExtInc 1,
        BfExtMoveLeft s,
        BfExtLoopEnd,

        --  >s + > - ]
        BfExtMoveRight s,
        BfExtInc 1,
        BfExtMoveRight 1,
        BfExtDec 1,
        BfExtLoopEnd
      ]

    -- -- [ > [ - <3 + < + >4 ] >N [ - ] <N <3 [ - >3 >N + <N <3 ] >2 [ - > + < ] < [ - > + < ] > + > - ]
    -- arrayCopyFwd :: InstCopyDir -> Int -> Int -> [BrainfuckExt]
    -- arrayCopyFwd dir n s =
    --   [ -- [ > [ - <3 + < + >4 ]
    --     BfExtLoopBegin,
    --     BfExtMoveRight 1,
    --     BfExtLoopBegin,
    --     BfExtDec 1,
    --     BfExtMoveLeft 3,
    --     BfExtInc 1,
    --     BfExtMoveLeft 1,
    --     BfExtInc 1,
    --     BfExtMoveRight 4,
    --     BfExtLoopEnd,
    --
    --     -- (>|<)N [ - ] (<|>)N <3 [ - >3 (>|<)N + (<|>)N <3 ]
    --     case dir of 
    --       DirRight -> BfExtMoveRight n
    --       DirLeft -> BfExtMoveLeft n,
    --     BfExtLoopBegin,
    --     BfExtDec 1,
    --     BfExtLoopEnd,
    --     case dir of 
    --       DirRight -> BfExtMoveLeft n
    --       DirLeft -> BfExtMoveRight n,
    --     BfExtMoveLeft 3,
    --     BfExtLoopBegin,
    --     BfExtDec 1,
    --     BfExtMoveRight 3,
    --     case dir of 
    --       DirRight -> BfExtMoveRight n
    --       DirLeft -> BfExtMoveLeft n,
    --     BfExtInc 1,
    --     case dir of 
    --       DirRight -> BfExtMoveLeft n
    --       DirLeft -> BfExtMoveRight n,
    --     BfExtMoveLeft 3,
    --     BfExtLoopEnd,
    --      
    --     -- >2 [ - > + < ]
    --     BfExtMoveRight 2,
    --     BfExtLoopBegin,
    --     BfExtDec 1,
    --     BfExtMoveRight 1,
    --     BfExtInc 1,
    --     BfExtMoveLeft 1,
    --     BfExtLoopEnd,
    --
    --     -- < [ - > + < ]
    --     BfExtMoveLeft 1,
    --     BfExtLoopBegin,
    --     BfExtDec 1,
    --     BfExtMoveRight 1,
    --     BfExtInc 1,
    --     BfExtMoveLeft 1,
    --     BfExtLoopEnd,
    --
    --     --  > + > - ]
    --     BfExtMoveRight 1,
    --     BfExtInc 1,
    --     BfExtMoveRight 1,
    --     BfExtDec 1,
    --     BfExtLoopEnd
    --   ]

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
    convert' (InstArrGet (Var i) s) =
        [ -- >i >3
          BfExtMoveRight i,
          BfExtMoveRight $ 2 * s + 1
        ] ++
        arrayFwd s ++ 
        arrayGet s ++ 
        arrayBck s ++ 
        [ -- <2 <i
          BfExtMoveLeft $ 2 * s,
          BfExtMoveLeft i
        ]
    convert' (InstArrSet (Var i) s) =
        [ -- >i >3
          BfExtMoveRight i,
          BfExtMoveRight $ 2 * s + 1
        ] ++
        arrayFwd s ++
        arraySet s ++ 
        arrayBck s ++ 
        [ -- <2 <i
          BfExtMoveLeft $ 2 * s,
          BfExtMoveLeft i
        ]
    convert' (InstArrCopy (Var i) (Var j) s) =
        let (dir, n) = if j > i then (DirRight, j - i) else (DirLeft, i - j) in
        [ -- >i >3
          BfExtMoveRight i,
          BfExtMoveRight $ 2 * s + 1
        ] ++
        arrayCopyFwd dir n s ++ 
        [ BfExtMoveLeft 1 ] ++
        arrayBck s ++ 
        [ -- <2 <i
          BfExtMoveLeft $ 2 * s,
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
        instToString (InstArrGet v s) = write $ "get{" ++ show s ++ "} " ++ varToString v
        instToString (InstArrSet v s) = write $ "set{" ++ show s ++ "} " ++ varToString v
        instToString (InstArrCopy f t s) = write $ varToString t ++ "{" ++ show s ++ "}" ++ "[] = " ++ varToString f ++ "[]"

        varToString (Var i) = "%" ++ show i
