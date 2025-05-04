module BasicExt where

import Basic

-- Also basic but with more powerfull commands
-- `+n` -- `+` n times
-- `-n` -- `-` n times
-- `>n` -- `>` n times
-- `<n` -- `<` n times
-- `,`, `.`, `[`, `]` -- the same as in Basic

data BrainfuckExt = BfExtInc Int
                  | BfExtDec Int
                  | BfExtMoveRight Int
                  | BfExtMoveLeft Int
                  | BfExtRead
                  | BfExtWrite
                  | BfExtLoopBegin
                  | BfExtLoopEnd

optimize :: [BrainfuckExt] -> [BrainfuckExt]
optimize = opt'
    where
        opt' insts = 
            let insts' = opt insts in
            if length insts' == length insts
            then insts'
            else opt' insts'
        
        opt (BfExtMoveRight x : BfExtMoveRight y : insts) = opt $ BfExtMoveRight (x + y) : insts
        opt (BfExtMoveLeft x : BfExtMoveLeft y : insts) = opt $ BfExtMoveLeft (x + y) : insts
        opt (BfExtMoveRight x : BfExtMoveLeft y : insts)
            | x > y = opt $ BfExtMoveRight (x - y) : insts
            | otherwise = opt $ BfExtMoveLeft (y - x) : insts
        opt (BfExtMoveLeft x : BfExtMoveRight y : insts)
            | x > y = opt $ BfExtMoveLeft (x - y) : insts
            | otherwise = opt $ BfExtMoveRight (y - x) : insts
        opt (BfExtInc 0 : insts) = opt insts
        opt (BfExtDec 0 : insts) = opt insts
        opt (BfExtMoveRight 0 : insts) = opt insts
        opt (BfExtMoveLeft 0 : insts) = opt insts
        opt (inst : insts) = inst : opt insts
        opt [] = []

convert :: [BrainfuckExt] -> [Brainfuck]
convert = concatMap convert'
    where
        convert' :: BrainfuckExt -> [Brainfuck]
        convert' (BfExtInc n) = replicate n BfInc
        convert' (BfExtDec n) = replicate n BfDec
        convert' (BfExtMoveRight n) = replicate n BfMoveRight
        convert' (BfExtMoveLeft n) = replicate n BfMoveLeft
        convert' BfExtRead = [BfRead]
        convert' BfExtWrite = [BfWrite]
        convert' BfExtLoopBegin = [BfLoopBegin]
        convert' BfExtLoopEnd = [BfLoopEnd]

progToString :: [BrainfuckExt] -> String
progToString = instsToString
    where
        instsToString :: [BrainfuckExt] -> String
        instsToString block = unwords $ fmap instToString block

        instToString :: BrainfuckExt -> String
        instToString (BfExtInc n) = "+" ++ show n
        instToString (BfExtDec n) = "-" ++ show n
        instToString (BfExtMoveRight n) = ">" ++ show n
        instToString (BfExtMoveLeft n) = "<" ++ show n
        instToString BfExtRead = ","
        instToString BfExtWrite = "."
        instToString BfExtLoopBegin = "["
        instToString BfExtLoopEnd = "]"
