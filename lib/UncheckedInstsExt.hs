module UncheckedInstsExt where

import Data.List (intercalate, intersperse)
import UncheckedInsts (UncheckedInst (..))
import qualified UncheckedInsts as UI
import ProgWriter
import Control.Monad (forM_)

-- More high level version of Inst with labels and gotos
-- The structure of program:
-- ```
-- label[0]: #0
-- label[1]: #1
-- ...
-- label[n]: #n
-- ```
--
-- ```
-- %pc = n
--
-- if %pc != 0 {
--     %pc -= 1
--     if pc != 0 {
--         %pc -= 1
--         if pc != 0 {
--            ...
--         } else {
--           #2
--         }
--     } else {
--       #1
--     }
-- } else {
--     #0
-- }
-- ```
-- - Also exiting when %pc = 0
-- - Each block should end with goto
-- - goto must always be last instruction
--
-- CopyAdd:
-- %j1 = %i; %j2 = %i; ...
-- is
-- %tmp = %i; %j1 = %i; %j2 = %i; ...
-- %i = %tmp
--
-- Same for CopySub
--
-- If:
-- if (%i != 0) #j #k
-- is
-- %tmpa = %i
-- %tmpb = 1
-- while (%tmpa != 0) {
--   #j
--   %tmpa = 0
--   %tmpb = 0
-- }
-- while (%tmpb != 0) {
--   #k
--   %tmpb = 0
-- }
--
-- Layout for programm

-- | 0  | 1    | 2    | ...
-- | pc | tmpa | tmpb | ...
data Var = ArrTargetVar Int Int | Var Int | Pc | TmpA | TmpB

convertVar :: Var -> UI.Var
convertVar (ArrTargetVar i s) = UI.arrayTargetVar (convertVar $ Var i) s
convertVar (Var i) = UI.Var $ i + 3
convertVar Pc = UI.Var 0
convertVar TmpA = UI.Var 1
convertVar TmpB = UI.Var 2

data Lbl = Lbl Int | Exit

transLbl :: Lbl -> Int
transLbl (Lbl i) = i + 1
transLbl Exit = 0

data UncheckedInstExt
  = InsExtGoto Lbl
  | InsExtConst Var Int
  | InsExtCopyAdd Var [Var]
  | InsExtCopySub Var [Var]
  | InsExtBranch Var Lbl Lbl
  | InsExtRead Var
  | InsExtWrite Var
  | InsExtIntrinsic [UncheckedInst]
  | InsExtArrayCopy Var Var Int Int
  | InsExtArrayGet Var Var Int
  | InsExtArraySet Var Var Int

convert :: [[UncheckedInstExt]] -> [UncheckedInst]
convert = convertBlocks
  where
    convert' :: UncheckedInstExt -> [UncheckedInst]
    convert' (InsExtGoto l) =
      [ InstConst (convertVar TmpA) (transLbl l),
        InstMoveAdd (convertVar TmpA) [convertVar Pc]
      ]
    convert' (InsExtConst var n) = [InstConst (convertVar var) n]
    convert' (InsExtCopyAdd v vs) =
      [ InstConst (convertVar TmpA) 0,
        InstMoveAdd (convertVar v) (convertVar TmpA : fmap convertVar vs),
        InstMoveAdd (convertVar TmpA) [convertVar v]
      ]
    convert' (InsExtCopySub v vs) =
      [ InstConst (convertVar TmpA) 0,
        InstConst (convertVar TmpB) 0,
        InstMoveAdd (convertVar v) [convertVar TmpA, convertVar TmpB],
        InstMoveAdd (convertVar TmpA) [convertVar v],
        InstMoveSub (convertVar v) (fmap convertVar vs),
        InstMoveAdd (convertVar TmpB) [convertVar v]
      ]
    convert' (InsExtBranch v thenLbl elseLbl) = mkIf v (convert' $ InsExtGoto thenLbl) (convert' $ InsExtGoto elseLbl)
    convert' (InsExtRead v) = [InstRead (convertVar v)]
    convert' (InsExtWrite v) = [InstWrite (convertVar v)]
    convert' (InsExtIntrinsic prog) = prog
    convert' (InsExtArrayCopy fv tv sz se) =
      fmap (\k -> InstConst (UI.arrayTargetVar' (convertVar fv) se k) 0) [0..se-1] ++
      [ InstConst (UI.arrayTargetIdxVar (convertVar fv) se) sz,
        InstArrCopy (convertVar fv) (convertVar tv) se
      ]
    convert' (InsExtArrayGet av iv s) =
      [ InstConst (convertVar TmpA) 0,
        InstMoveAdd (convertVar iv) [convertVar TmpA, UI.arrayTargetIdxVar (convertVar av) s],
        InstMoveAdd (convertVar TmpA) [convertVar iv],
        InstArrGet (convertVar av) s
      ]
    convert' (InsExtArraySet av iv s) =
      [ InstConst (convertVar TmpA) 0,
        InstMoveAdd (convertVar iv) [convertVar TmpA, UI.arrayTargetIdxVar (convertVar av) s],
        InstMoveAdd (convertVar TmpA) [convertVar iv],
        InstArrSet (convertVar av) s
      ]

    convertBlock :: [UncheckedInstExt] -> [UncheckedInst]
    convertBlock [] = []
    convertBlock (inst : insts) = convert' inst ++ convertBlock insts

    convertBlocks :: [[UncheckedInstExt]] -> [UncheckedInst]
    convertBlocks [] = error "Program must contain at least one block"
    convertBlocks bs =
      [ InstConst (convertVar Pc) 1, -- entry is first block
        InstWhile
          (convertVar Pc) -- goto 0 is exit
          (convertBlocks' bs)
      ]

    convertBlocks' :: [[UncheckedInstExt]] -> [UncheckedInst]
    convertBlocks' [] = [InstConst (convertVar Pc) 0] -- exit on unknown label
    convertBlocks' (b : bs) =
      [ InstConst (convertVar TmpA) 1,
        InstMoveSub (convertVar TmpA) [convertVar Pc]
      ]
        ++ mkIf
          Pc
          (convertBlocks' bs)
          (convertBlock b)

    mkIf :: Var -> [UncheckedInst] -> [UncheckedInst] -> [UncheckedInst]
    mkIf var thenBr elseBr =
      [ InstConst (convertVar TmpA) 0,
        InstConst (convertVar TmpB) 0,
        InstMoveAdd (convertVar var) [convertVar TmpA, convertVar TmpB],
        InstMoveAdd (convertVar TmpB) [convertVar var],
        InstConst (convertVar TmpB) 1,
        InstWhile
          (convertVar TmpA)
          ( thenBr
              ++ [ InstConst (convertVar TmpA) 0,
                   InstConst (convertVar TmpB) 0
                 ]
          ),
        InstWhile
          (convertVar TmpB)
          ( elseBr
              ++ [ InstConst (convertVar TmpB) 0
                 ]
          )
      ]

-- optimize :: [[UncheckedInstExt]] -> [[UncheckedInstExt]]
-- optimize = undefined
--     where
--         -- transBlocks :: Int -> Int -> M.Map Int Int -> [[UncheckedInstExt]] -> ([[UncheckedInstExt]], M.Map Int Int)
--         -- transBlocks removed i substs ([InsExtGoto j] : blocks)
--
--         substTrans :: [[UncheckedInstExt]] -> [[UncheckedInstExt]]
--         substTrans
--
--         transClos :: M.Map Int Int -> M.Map Int Int
--         transClos g = makeSubsts' g
--             where
--                 collectIncoming' :: M.Map Int Int -> S.Set Int
--                 collectIncoming' = M.foldr S.insert S.empty
--
--                 makeSubsts :: M.Map Int Int -> Int -> M.Map Int Int -> (M.Map Int Int, Int)
--                 makeSubsts substInit node g = case M.lookup node g of
--                     Just node' ->
--                         let (res, fin) = makeSubsts substInit node' g in
--                         (M.insert node fin res, fin)
--                     Nothing -> (substInit, node)
--
--                 makeSubsts' :: M.Map Int Int -> M.Map Int Int
--                 makeSubsts' g =
--                     let incoming = collectIncoming' g in
--                     M.foldlWithKey (\substs i j ->
--                         if S.member i incoming then substs
--                         else fst $ makeSubsts substs i g)
--                         M.empty g
--
--         collectTransitive :: Int -> [[UncheckedInstExt]] -> M.Map Int Int
--         collectTransitive i ([InsExtGoto (Lbl j)] : blocks) =
--             let substs = collectTransitive (i + 1) blocks in
--             M.insert i j substs
--         collectTransitive i (b : bs) = collectTransitive (i + 1) bs
--         collectTransitive _ [] = M.empty

progToString :: [[UncheckedInstExt]] -> String
progToString prog = runWriter $ progToString' prog
  where
    progToString' :: [[UncheckedInstExt]] -> ProgWriter ()
    progToString' prog' = forM_ (intersperse nl $ zipWith blockToString [0..] prog') id

    blockToString :: Int -> [UncheckedInstExt] -> ProgWriter ()
    blockToString blockIdx block = do
        write $ "#" ++ show blockIdx ++ ":"
        withIndent $ nl >> instsToString block

    instsToString :: [UncheckedInstExt] -> ProgWriter ()
    instsToString block = forM_ (intersperse nl $ fmap instToString block) id

    lblToString :: Lbl -> String
    lblToString (Lbl i) = "#" ++ show i
    lblToString Exit = "#exit"

    instToString ::  UncheckedInstExt -> ProgWriter ()
    instToString (InsExtConst var n) = write $ varToString var ++ " := " ++ show n
    instToString (InsExtGoto l) = write $ "goto " ++ lblToString l
    instToString (InsExtCopyAdd v vs) =
        write $ intercalate "; " (fmap (\v' -> varToString v' ++ " += " ++ varToString v) vs)
    instToString (InsExtCopySub v vs) =
        write $ intercalate "; " (fmap (\v' -> varToString v' ++ " -= " ++ varToString v) vs)
    instToString (InsExtBranch v thenLbl elseLbl) = do
        write $ "if " ++ varToString v ++ " != 0 {"
        withIndent $ nl >> instsToString [InsExtGoto thenLbl]
        nl >> write "} else {"
        withIndent $ nl >> instsToString  [InsExtGoto elseLbl]
        nl >> write "}"
    instToString (InsExtRead v) = write $ "read " ++ varToString v
    instToString (InsExtWrite v) = write $ "write " ++ varToString v
    instToString (InsExtIntrinsic prog') = do
        write "Intrinsic:"
        withIndent $ nl >> UI.progToString' prog'
    instToString (InsExtArrayCopy fv tv sz s) =
        write $ varToString tv ++ "{" ++ show s ++ "}[0.." ++ show sz ++ "] = " ++ varToString fv ++ "[0.." ++ show sz ++ "]"
    instToString (InsExtArrayGet av iv s) =
        write $ "set{" ++ show s ++ "} " ++ varToString av ++ "[" ++ varToString iv ++ "]"
    instToString (InsExtArraySet av iv s) =
        write $  "get{" ++ show s ++ "} " ++ varToString av ++ "[" ++ varToString iv ++ "]"

    varToString (Var i) = "%" ++ show i
    varToString (ArrTargetVar i s) = "%" ++ show i ++ "{" ++ show s ++ "}.target"
    varToString Pc = "%pc"
    varToString TmpA = "%tmpa"
    varToString TmpB = "%tmpb"
