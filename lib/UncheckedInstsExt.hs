module UncheckedInstsExt where

import Data.List (intercalate)
import UncheckedInsts (UncheckedInst (..))
import qualified UncheckedInsts as UI
import qualified Data.Map as M
import qualified Data.Set as S

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

varToIdx :: Var -> Int
varToIdx (Var i) = i + 3
varToIdx Pc = 0
varToIdx TmpA = 1
varToIdx TmpB = 2

transVar :: Var -> UI.Var
transVar v = UI.Var $ varToIdx v

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

convert :: [[UncheckedInstExt]] -> [UncheckedInst]
convert = convertBlocks
  where
    convert' :: UncheckedInstExt -> [UncheckedInst]
    convert' (InsExtGoto l) =
      [ InstConst (transVar TmpA) (transLbl l),
        InstMoveAdd (transVar TmpA) [transVar Pc]
      ]
    convert' (InsExtConst var n) = [InstConst (transVar var) n]
    convert' (InsExtCopyAdd v vs) =
      [ InstConst (transVar TmpA) 0,
        InstMoveAdd (transVar v) (transVar TmpA : fmap transVar vs),
        InstMoveAdd (transVar TmpA) [transVar v]
      ]
    convert' (InsExtCopySub v vs) =
      [ InstConst (transVar TmpA) 0,
        InstConst (transVar TmpB) 0,
        InstMoveAdd (transVar v) [transVar TmpA, transVar TmpB],
        InstMoveAdd (transVar TmpA) [transVar v],
        InstMoveSub (transVar v) (fmap transVar vs),
        InstMoveAdd (transVar TmpB) [transVar v]
      ]
    convert' (InsExtBranch v thenLbl elseLbl) = mkIf v (convert' $ InsExtGoto thenLbl) (convert' $ InsExtGoto elseLbl)
    convert' (InsExtRead v) = [InstRead (transVar v)]
    convert' (InsExtWrite v) = [InstWrite (transVar v)]
    convert' (InsExtIntrinsic prog) = prog

    convertBlock :: [UncheckedInstExt] -> [UncheckedInst]
    convertBlock [] = []
    convertBlock (inst : insts) = convert' inst ++ convertBlock insts

    convertBlocks :: [[UncheckedInstExt]] -> [UncheckedInst]
    convertBlocks [] = error "Program must contain at least one block"
    convertBlocks bs =
      [ InstConst (transVar Pc) 1, -- entry is first block
        InstWhile
          (transVar Pc) -- goto 0 is exit
          (convertBlocks' bs)
      ]

    convertBlocks' :: [[UncheckedInstExt]] -> [UncheckedInst]
    convertBlocks' [] = [InstConst (transVar Pc) 0] -- exit on unknown label
    convertBlocks' (b : bs) =
      [ InstConst (transVar TmpA) 1,
        InstMoveSub (transVar TmpA) [transVar Pc]
      ]
        ++ mkIf
          Pc
          (convertBlocks' bs)
          (convertBlock b)

    mkIf :: Var -> [UncheckedInst] -> [UncheckedInst] -> [UncheckedInst]
    mkIf var thenBr elseBr =
      [ InstConst (transVar TmpA) 0,
        InstConst (transVar TmpB) 0,
        InstMoveAdd (transVar var) [transVar TmpA, transVar TmpB],
        InstMoveAdd (transVar TmpB) [transVar var],
        InstConst (transVar TmpB) 1,
        InstWhile
          (transVar TmpA)
          ( thenBr
              ++ [ InstConst (transVar TmpA) 0,
                   InstConst (transVar TmpB) 0
                 ]
          ),
        InstWhile
          (transVar TmpB)
          ( elseBr
              ++ [ InstConst (transVar TmpB) 0
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
progToString prog = intercalate "\n" $ zipWith blockToString [0 ..] prog
  where
    getIndent :: Int -> String
    getIndent indent = replicate (indent * 2) ' '

    blockToString :: Int -> [UncheckedInstExt] -> String
    blockToString blockIdx block = "#" ++ show blockIdx ++ ":\n" ++ instsToString 1 block

    instsToString :: Int -> [UncheckedInstExt] -> String
    instsToString indent block = intercalate "\n" $ fmap (instToString indent) block

    lblToString :: Lbl -> String
    lblToString (Lbl i) = "#" ++ show i
    lblToString Exit = "#exit"

    instToString :: Int -> UncheckedInstExt -> String
    instToString indent (InsExtConst var n) = getIndent indent ++ varToString var ++ " := " ++ show n
    instToString indent (InsExtGoto l) = getIndent indent ++ "goto " ++ lblToString l
    instToString indent (InsExtCopyAdd v vs) =
      getIndent indent
        ++ intercalate "; " (fmap (\v' -> varToString v' ++ " += " ++ varToString v) vs)
    instToString indent (InsExtCopySub v vs) =
      getIndent indent
        ++ intercalate "; " (fmap (\v' -> varToString v' ++ " -= " ++ varToString v) vs)
    instToString indent (InsExtBranch v thenLbl elseLbl) =
      getIndent indent
        ++ "if "
        ++ varToString v
        ++ " != 0 {\n"
        ++ instsToString (indent + 1) [InsExtGoto thenLbl]
        ++ "\n"
        ++ getIndent indent
        ++ "} else {\n"
        ++ instsToString (indent + 1) [InsExtGoto elseLbl]
        ++ "\n"
        ++ getIndent indent
        ++ "}"
    instToString indent (InsExtRead v) = getIndent indent ++ "read " ++ varToString v
    instToString indent (InsExtWrite v) = getIndent indent ++ "write " ++ varToString v
    instToString indent (InsExtIntrinsic prog') = getIndent indent ++ "Intrinsic:\n" ++ UI.progToStringWithIndent (indent + 1) prog'

    varToString (Var i) = "%" ++ show i
    varToString Pc = "%pc"
    varToString TmpA = "%tmpa"
    varToString TmpB = "%tmpb"
