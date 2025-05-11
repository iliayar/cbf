{-# LANGUAGE NamedFieldPuns #-}

module UncheckedProc where

import BasicExt (BrainfuckExt (..))
import Control.Monad (forM_, unless, when)
import qualified Control.Monad.State as ST
import Data.List (intercalate)
import qualified Data.Map as M
import UncheckedInsts (UncheckedInst (..))
import UncheckedInstsExt (UncheckedInstExt (..))
import qualified UncheckedInstsExt as UIE
import ProgWriter

-- The version of UncheckedInstsExt but with procedures
-- Introducing procedures and calls
--
-- Procedure is just an labeled block. Blocks are now numbered inside one function
--
-- main:
--   #0:
--     foo()
--
-- foo:
--   #0:
--     #foo
--
-- is
-- #1:
--   %ret = #2
--   goto #i
-- #2:
--   goto #exit
--
-- main:
-- #i:
--   frame enter N -- intrinsic: >N
--   %ret = #(i+1)
--   goto #j
-- #(i+1):
--   frame exit N  -- intrinsic: <N
--   goto #(i+2)
-- #(i+2):
--   ret           -- instrinsic: %pc = %ret
--
-- foo:
-- #j:
--   #foo
--   goto #(j+1)
-- #(j+1)
--   ret
--
-- goto on this level are not the same: Here goto implicitly splits to two blocks
-- because previously goto will do nothing if it's not last in the block
--
-- every block that doesn't ends with branch or goto, will implicitly get it

data Var = Var Int | RetVar

convertVar :: Var -> UIE.Var
convertVar (Var i) = UIE.Var $ i + 1
-- convertVar (ArrTargetVar i s) = UIE.convertRef $ UIE.RefArrayValue (UIE.RefVar $ UIE.Var $ i + 1) s
convertVar RetVar = UIE.Var 0

data Ref = RefVar Var | RefArrayValue Ref Int | RefStructField Ref Int

convertRef :: Ref -> UIE.Ref
convertRef (RefVar v) = UIE.RefVar $ convertVar v
convertRef (RefArrayValue ref i) = UIE.RefArrayValue (convertRef ref) i
convertRef (RefStructField ref o) = UIE.RefStructField (convertRef ref) o

-- mkArrTargetVar :: UIE.Var -> UIE.Var
-- mkArrTargetVar (Var i) = UIE.Var $ 

data Lbl = Lbl Int | Exit | Ret

newtype Func = Func Int

data UncheckedProc
  = ProcGoto Lbl
  | ProcBranch Ref Lbl Lbl
  | ProcConst Ref Int
  | ProcCopyAdd Ref [Ref]
  | ProcCopySub Ref [Ref]
  | ProcRead Ref
  | ProcWrite Ref
  | ProcCall Func Int
  | ProcArrayCopy Ref Ref Int Int
  | ProcArrayGet Ref Ref Int
  | ProcArraySet Ref Ref Int

isTerminator :: UncheckedInstExt -> Bool
isTerminator (InsExtGoto _) = True
isTerminator (InsExtBranch {}) = True
isTerminator (InsExtCopyAdd _ [UIE.RefVar UIE.Pc]) = True
isTerminator (InsExtConst (UIE.RefVar UIE.Pc) _) = True
isTerminator _ = False

data ConverterState = ConverterState
  { blocksMapping :: M.Map (Int, Int) Int,
    functionsMapping :: M.Map Int Int,
    functionsRet :: M.Map Int Int,
    blocksCnt :: Int,
    reservedBlocks :: Int,
    blocks :: [Resolver [UncheckedInstExt]]
  }

type Converter = ST.State ConverterState

reserveBlocks' :: Int -> Converter Int
reserveBlocks' toReserve = do
  st@(ConverterState {blocksCnt, reservedBlocks}) <- ST.get
  ST.put $ st {reservedBlocks = max reservedBlocks toReserve}
  return blocksCnt

nextBlock' :: Converter Int
nextBlock' = do
  ConverterState {blocksCnt} <- ST.get
  return $ blocksCnt + 1

addFallthroughIfNoTerminal :: Converter ()
addFallthroughIfNoTerminal = do
  st@(ConverterState {blocks, blocksCnt}) <- ST.get
  case blocks of
    [] -> return ()
    (block : blocks') -> do
      let block' = do
            insts <- block
            if null insts || not (isTerminator $ last insts)
              then return $ insts ++ [InsExtGoto $ UIE.Lbl blocksCnt]
              else return insts
      ST.put $ st {blocks = block' : blocks'}

createBlock :: Converter ()
createBlock = do
  addFallthroughIfNoTerminal
  st@(ConverterState {blocks, reservedBlocks, blocksCnt}) <- ST.get
  when (reservedBlocks == 0) $ error "Trying create block when it's not reserved"
  ST.put $
    st
      { blocks = return [] : blocks,
        reservedBlocks = reservedBlocks - 1,
        blocksCnt = blocksCnt + 1
      }

addInstructions :: Resolver [UncheckedInstExt] -> Converter ()
addInstructions newInsts = do
  st@(ConverterState {blocks}) <- ST.get
  case blocks of
    [] -> error "No blocks to add instruction to"
    (block : blocks') -> do
      let block' = do
            insts <- block
            newInsts' <- newInsts
            return $ insts ++ newInsts'
      ST.put $ st {blocks = block' : blocks'}

addInstructionsPure :: [UncheckedInstExt] -> Converter ()
addInstructionsPure = addInstructions . return

recordBlock :: Int -> Int -> Int -> Converter ()
recordBlock funcIdx blockIdx i = do
  st@(ConverterState {blocksMapping}) <- ST.get
  ST.put $ st {blocksMapping = M.insert (funcIdx, blockIdx) i blocksMapping}

recordFunc :: Int -> Int -> Converter ()
recordFunc funcIdx i = do
  st@(ConverterState {functionsMapping}) <- ST.get
  ST.put $ st {functionsMapping = M.insert funcIdx i functionsMapping}

recordFuncRet :: Int -> Int -> Converter ()
recordFuncRet funcIdx i = do
  st@(ConverterState {functionsRet}) <- ST.get
  ST.put $ st {functionsRet = M.insert funcIdx i functionsRet}

data ResolverState = ResolverState
  { rBlocksMapping :: M.Map (Int, Int) Int,
    rFunctionsMapping :: M.Map Int Int,
    rFunctionsRet :: M.Map Int Int
  }

type Resolver = ST.State ResolverState

resolveLbl :: Int -> Lbl -> Resolver UIE.Lbl
resolveLbl funcIdx (Lbl blockIdx) = do
  ResolverState {rBlocksMapping} <- ST.get
  case M.lookup (funcIdx, blockIdx) rBlocksMapping of
    Just i -> return $ UIE.Lbl i
    Nothing -> error $ "Unresolved lbl to block #" ++ show blockIdx ++ " in function " ++ show funcIdx
resolveLbl _ Exit = return UIE.Exit
resolveLbl funcIdx Ret = do
  ResolverState {rFunctionsRet} <- ST.get
  case M.lookup funcIdx rFunctionsRet of
    Just i -> return $ UIE.Lbl i
    Nothing -> error $ "Unresolved return block for function " ++ show funcIdx

resolveCall :: Int -> Resolver UIE.Lbl
resolveCall funcIdx = do
  ResolverState {rFunctionsMapping} <- ST.get
  case M.lookup funcIdx rFunctionsMapping of
    Just i -> return $ UIE.Lbl i
    Nothing -> error $ "Unresolved call to function " ++ show funcIdx

convert :: [[[UncheckedProc]]] -> [[UncheckedInstExt]]
convert proc =
  let (_, converter) =
        ST.runState (convert' proc) $
          ConverterState
            { blocksMapping = M.empty,
              functionsMapping = M.empty,
              functionsRet = M.empty,
              blocks = [],
              blocksCnt = 0,
              reservedBlocks = 0
            }
   in let resolver =
            ResolverState
              { rBlocksMapping = blocksMapping converter,
                rFunctionsMapping = functionsMapping converter,
                rFunctionsRet = functionsRet converter
              }
       in (\r -> fst $ ST.runState r resolver) <$> reverse (blocks converter)
  where
    convert' :: [[[UncheckedProc]]] -> Converter ()
    convert' proc' = do
      i <- reserveBlocks' 2
      createBlock
      addInstructions $ do
        firstFunc <- resolveCall 0
        return
          [ InsExtConst (convertRef $ RefVar RetVar) (i + 1),
            InsExtGoto firstFunc
          ]
      createBlock
      addInstructionsPure [InsExtGoto UIE.Exit]
      forM_ (zip [0 ..] proc') (uncurry convertFunction')

    convertFunction' :: Int -> [[UncheckedProc]] -> Converter ()
    convertFunction' funcIdx func = do
      i <- reserveBlocks' 1
      createBlock
      recordFunc funcIdx i
      addInstructions $ do
        firstLbl <- resolveLbl funcIdx (Lbl 0)
        return [InsExtGoto firstLbl]
      forM_ (zip [0 ..] func) (uncurry (convertBlock' funcIdx))
      retBlockIdx <- reserveBlocks' 1
      createBlock
      recordFuncRet funcIdx retBlockIdx
      addInstructionsPure
        [ -- FIXME: Call the plumber, we are leaking hard
          InsExtConst (UIE.RefVar UIE.Pc) $ UIE.transLbl $ UIE.Lbl 0,
          InsExtCopyAdd (convertRef $ RefVar RetVar) [UIE.RefVar UIE.Pc]
        ]

    convertBlock' :: Int -> Int -> [UncheckedProc] -> Converter ()
    convertBlock' funcIdx blockIdx block = do
      i <- reserveBlocks' 1
      createBlock
      recordBlock funcIdx blockIdx i
      forM_ block (convertInst' funcIdx)

    convertInst' :: Int -> UncheckedProc -> Converter ()
    convertInst' funcIdx (ProcGoto lbl) = do
      _ <- reserveBlocks' 1
      addInstructions $ do
        lbl' <- resolveLbl funcIdx lbl
        return [InsExtGoto lbl']
      createBlock
    convertInst' funcIdx (ProcBranch r thenLbl elseLbl) = do
      _ <- reserveBlocks' 1
      addInstructions $ do
        thenLbl' <- resolveLbl funcIdx thenLbl
        elseLbl' <- resolveLbl funcIdx elseLbl
        return [InsExtBranch (convertRef r) thenLbl' elseLbl']
      createBlock 
    convertInst' _ (ProcConst r n) = addInstructionsPure [InsExtConst (convertRef r) n]
    convertInst' _ (ProcCopyAdd r rs) =
      addInstructionsPure [InsExtCopyAdd (convertRef r) (fmap convertRef rs)]
    convertInst' _ (ProcCopySub r rs) =
      addInstructionsPure [InsExtCopySub (convertRef r) (fmap convertRef rs)]
    convertInst' _ (ProcRead r) = addInstructionsPure [InsExtRead $ convertRef r]
    convertInst' _ (ProcWrite r) = addInstructionsPure [InsExtWrite $ convertRef r]
    convertInst' _ (ProcCall (Func i) n) = do
      retLbl <- reserveBlocks' 1
      addInstructions $ do
        funcLbl <- resolveCall i
        return
          [ InsExtIntrinsic [InstIntrinsic [BfExtMoveRight n]],
            InsExtConst (convertRef $ RefVar RetVar) retLbl,
            InsExtGoto funcLbl
          ]
      createBlock
      addInstructionsPure
        [InsExtIntrinsic [InstIntrinsic [BfExtMoveLeft n]]]
      addInstructions (return [])
    convertInst' _ (ProcArrayCopy fr tr sz s) = addInstructionsPure [InsExtArrayCopy (convertRef fr) (convertRef tr) sz s]
    convertInst' _ (ProcArrayGet ar ir s) = addInstructionsPure [InsExtArrayGet (convertRef ar) (convertRef ir) s]
    convertInst' _ (ProcArraySet ar ir s) = addInstructionsPure [InsExtArraySet (convertRef ar) (convertRef ir) s]

progToString :: [[[UncheckedProc]]] -> String
progToString prog = runWriter $ progToString' prog
  where
    progToString' :: [[[UncheckedProc]]] -> ProgWriter ()
    progToString' prog' = forM_ (zip [0 ..] prog') (\(i, f) -> unless (i == 0) nl >> functionToString i f)

    functionToString :: Int -> [[UncheckedProc]] -> ProgWriter ()
    functionToString funcIdx func = do
      write $ "func " ++ show funcIdx ++ " {"
      withIndent $ do
        nl
        forM_ (zip [0 ..] func) (\(i, b) -> unless (i == 0) nl >> blockToString i b)
      nl >> write "}"

    blockToString :: Int -> [UncheckedProc] -> ProgWriter ()
    blockToString blockIdx block = do
      write $ "#" ++ show blockIdx ++ ":"
      withIndent $ do
        forM_ block (\inst -> nl >> instToString inst)

    lblToString :: Lbl -> String
    lblToString (Lbl i) = "#" ++ show i
    lblToString Exit = "#exit"
    lblToString Ret = "#ret"

    varToString :: Var -> String
    varToString (Var i) = "%" ++ show i
    varToString RetVar = "%ret"

    refToString (RefVar v) = varToString v
    refToString (RefArrayValue ref s) = refToString ref ++ "{" ++ show s ++ "}.target"
    refToString (RefStructField ref o) = refToString ref ++ "{" ++ show o ++ "}"

    funcToString :: Func -> String
    funcToString (Func i) = "func " ++ show i

    instToString :: UncheckedProc -> ProgWriter ()
    instToString (ProcConst ref n) = write $ refToString ref ++ " := " ++ show n
    instToString (ProcGoto l) = write $ "goto " ++ lblToString l
    instToString (ProcBranch ref thenLbl elseLbl) = do
      write $ "if " ++ refToString ref ++ " != 0 {"
      withIndent $ nl >> write ("goto " ++ lblToString thenLbl)
      nl >> write "} else {"
      withIndent $ nl >> write ("goto " ++ lblToString elseLbl)
      nl >> write "}"
    instToString (ProcCopyAdd r rs) =
      write $ intercalate "; " (fmap (\r' -> refToString r' ++ " += " ++ refToString r) rs)
    instToString (ProcCopySub r rs) =
      write $ intercalate "; " (fmap (\r' -> refToString r' ++ " -= " ++ refToString r) rs)
    instToString (ProcRead r) = write $ "read " ++ refToString r
    instToString (ProcWrite r) = write $ "write " ++ refToString r
    instToString (ProcCall f n) = write $ "call[" ++ show n ++ "] " ++ funcToString f
    instToString (ProcArrayCopy fr tr sz s) =
        write $ refToString tr ++ "{" ++ show s ++ "}[0.." ++ show sz ++ "] = " ++ refToString fr ++ "[0.." ++ show sz ++ "]"
    instToString (ProcArrayGet ar ir s) =
        write $ "set{" ++ show s ++ "}" ++ refToString ar ++ "[" ++ refToString ir ++ "]"
    instToString (ProcArraySet ar ir s) =
        write $  "get{" ++ show s ++ "}" ++ refToString ar ++ "[" ++ refToString ir ++ "]"
