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
data Var = Var Int | Pc | TmpA | TmpB

data Ref = RefVar Var | RefArrayValue Ref Int | RefStructField Ref Int

convertVar :: Var -> UI.Var
convertVar (Var i) = UI.Var $ i + 3
convertVar Pc = UI.Var 0
convertVar TmpA = UI.Var 1
convertVar TmpB = UI.Var 2

convertRef :: Ref -> UI.Var
convertRef (RefVar v) = convertVar v
convertRef (RefArrayValue ref s) = UI.arrayTargetVar (convertRef ref) s
convertRef (RefStructField ref o) = UI.structOffset (convertRef ref) o

-- mkArrTargetVar :: Var -> Int -> Var
-- mkArrTargetVar (Var i) s = UI.arrayTargetVar (UI.Var i) s
-- mkArrTargetVar _ _ = error "mkArrTargetVar allows only Var"

data Lbl = Lbl Int | Exit

transLbl :: Lbl -> Int
transLbl (Lbl i) = i + 1
transLbl Exit = 0

data UncheckedInstExt
  = InsExtGoto Lbl
  | InsExtConst Ref Int
  | InsExtCopyAdd Ref [Ref]
  | InsExtCopySub Ref [Ref]
  | InsExtBranch Ref Lbl Lbl
  | InsExtRead Ref
  | InsExtWrite Ref
  | InsExtIntrinsic [UncheckedInst]
  | InsExtArrayCopy Ref Ref Int Int
  | InsExtArrayGet Ref Ref Int
  | InsExtArraySet Ref Ref Int

convert :: [[UncheckedInstExt]] -> [UncheckedInst]
convert = convertBlocks
  where
    convert' :: UncheckedInstExt -> [UncheckedInst]
    convert' (InsExtGoto l) =
      [ InstConst (convertVar TmpA) (transLbl l),
        InstMoveAdd (convertVar TmpA) [convertVar Pc]
      ]
    convert' (InsExtConst var n) = [InstConst (convertRef var) n]
    convert' (InsExtCopyAdd r rs) =
      [ InstConst (convertVar TmpA) 0,
        InstMoveAdd (convertRef r) (convertVar TmpA : fmap convertRef rs),
        InstMoveAdd (convertVar TmpA) [convertRef r]
      ]
    convert' (InsExtCopySub r rs) =
      [ InstConst (convertVar TmpA) 0,
        InstConst (convertVar TmpB) 0,
        InstMoveAdd (convertRef r) [convertVar TmpA, convertVar TmpB],
        InstMoveAdd (convertVar TmpA) [convertRef r],
        InstMoveSub (convertRef r) (fmap convertRef rs),
        InstMoveAdd (convertVar TmpB) [convertRef r]
      ]
    convert' (InsExtBranch r thenLbl elseLbl) = mkIf r (convert' $ InsExtGoto thenLbl) (convert' $ InsExtGoto elseLbl)
    convert' (InsExtRead r) = [InstRead (convertRef r)]
    convert' (InsExtWrite r) = [InstWrite (convertRef r)]
    convert' (InsExtIntrinsic prog) = prog
    convert' (InsExtArrayCopy fr tr sz se) =
      [ InstConst (UI.arrayTmpIndexVar' (convertRef fr) se) 0] ++
      fmap (\k -> InstConst (UI.arrayTmpVar' (convertRef fr) se k) 0) [0..se-1] ++
      fmap (\k -> InstConst (UI.arrayTargetVar' (convertRef fr) se k) 0) [0..se-1] ++
      [ InstConst (UI.arrayTargetIdxVar (convertRef fr) se) sz,
        InstArrCopy (convertRef fr) (convertRef tr) se
      ]
    convert' (InsExtArrayGet ar ir s) =
      [ InstConst (UI.arrayTmpIndexVar' (convertRef ar) s) 0] ++
      fmap (\k -> InstConst (UI.arrayTmpVar' (convertRef ar) s k) 0) [0..s-1] ++
      fmap (\k -> InstConst (UI.arrayTargetVar' (convertRef ar) s k) 0) [0..s-1] ++
      [ InstConst (convertVar TmpA) 0,
        InstMoveAdd (convertRef ir) [convertVar TmpA, UI.arrayTargetIdxVar (convertRef ar) s],
        InstMoveAdd (convertVar TmpA) [convertRef ir],
        InstArrGet (convertRef ar) s
      ]
    convert' (InsExtArraySet ar ir s) =
      [ InstConst (UI.arrayTmpIndexVar' (convertRef ar) s) 0] ++
      fmap (\k -> InstConst (UI.arrayTmpVar' (convertRef ar) s k) 0) [0..s-1] ++
      [ InstConst (convertVar TmpA) 0,
        InstMoveAdd (convertRef ir) [convertVar TmpA, UI.arrayTargetIdxVar (convertRef ar) s],
        InstMoveAdd (convertVar TmpA) [convertRef ir],
        InstArrSet (convertRef ar) s
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
          (RefVar Pc)
          (convertBlocks' bs)
          (convertBlock b)

    mkIf :: Ref -> [UncheckedInst] -> [UncheckedInst] -> [UncheckedInst]
    mkIf ref thenBr elseBr =
      [ InstConst (convertVar TmpA) 0,
        InstConst (convertVar TmpB) 0,
        InstMoveAdd (convertRef ref) [convertVar TmpA, convertVar TmpB],
        InstMoveAdd (convertVar TmpB) [convertRef ref],
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
    instToString (InsExtConst ref n) = write $ refToString ref ++ " := " ++ show n
    instToString (InsExtGoto l) = write $ "goto " ++ lblToString l
    instToString (InsExtCopyAdd r rs) =
        write $ intercalate "; " (fmap (\r' -> refToString r' ++ " += " ++ refToString r) rs)
    instToString (InsExtCopySub r rs) =
        write $ intercalate "; " (fmap (\r' -> refToString r' ++ " -= " ++ refToString r) rs)
    instToString (InsExtBranch r thenLbl elseLbl) = do
        write $ "if " ++ refToString r ++ " != 0 {"
        withIndent $ nl >> instsToString [InsExtGoto thenLbl]
        nl >> write "} else {"
        withIndent $ nl >> instsToString  [InsExtGoto elseLbl]
        nl >> write "}"
    instToString (InsExtRead r) = write $ "read " ++ refToString r
    instToString (InsExtWrite r) = write $ "write " ++ refToString r
    instToString (InsExtIntrinsic prog') = do
        write "Intrinsic:"
        withIndent $ nl >> UI.progToString' prog'
    instToString (InsExtArrayCopy fr tr sz s) =
        write $ refToString tr ++ "{" ++ show s ++ "}[0.." ++ show sz ++ "] = " ++ refToString fr ++ "[0.." ++ show sz ++ "]"
    instToString (InsExtArrayGet ar ir s) =
        write $ "set{" ++ show s ++ "} " ++ refToString ar ++ "[" ++ refToString ir ++ "]"
    instToString (InsExtArraySet ar ir s) =
        write $  "get{" ++ show s ++ "} " ++ refToString ar ++ "[" ++ refToString ir ++ "]"

    varToString (Var i) = "%" ++ show i
    varToString Pc = "%pc"
    varToString TmpA = "%tmpa"
    varToString TmpB = "%tmpb"

    refToString (RefVar v) = varToString v
    refToString (RefArrayValue ref s) = "%" ++ refToString ref ++ "{" ++ show s ++ "}.target"
    refToString (RefStructField ref o) = "%" ++ refToString ref ++ "{" ++ show o ++ "}"
