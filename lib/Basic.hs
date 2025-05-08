module Basic where
import Data.Maybe (mapMaybe)

-- Just a basic Brainfuck
-- `,` -- Read to cursor
-- `.` -- Write from cursor
-- `>` -- Move cursor right
-- `<` -- Move cursor left
-- `+` -- Increment at cursor
-- `-` -- Decrement at cursor
-- `[` -- If cursor is 0 then skip to `]`
-- `]` -- If cursor is not 0 then return to `]`

data Brainfuck = BfRead
               | BfWrite
               | BfMoveRight
               | BfMoveLeft
               | BfInc
               | BfDec
               | BfLoopBegin
               | BfLoopEnd
    deriving (Show)

bfFromString :: String -> [Brainfuck]
bfFromString = mapMaybe bfFromChar
    where
        bfFromChar :: Char -> Maybe Brainfuck
        bfFromChar ',' = Just BfRead
        bfFromChar '.' = Just BfWrite
        bfFromChar '>' = Just BfMoveRight
        bfFromChar '<' = Just BfMoveLeft
        bfFromChar '+' = Just BfInc
        bfFromChar '-' = Just BfDec
        bfFromChar '[' = Just BfLoopBegin
        bfFromChar ']' = Just BfLoopEnd
        bfFromChar '\n' = Nothing
        bfFromChar ch = error $ "Unknown instruction: " ++ [ch]

bfToString :: [Brainfuck] -> String
bfToString = fmap bfToChar
    where
        bfToChar :: Brainfuck -> Char
        bfToChar BfRead = ','
        bfToChar BfWrite = '.'
        bfToChar BfMoveLeft = '<'
        bfToChar BfMoveRight = '>'
        bfToChar BfInc = '+'
        bfToChar BfDec = '-'
        bfToChar BfLoopBegin = '['
        bfToChar BfLoopEnd = ']'

-- Credits to https://esolangs.org/wiki/Brainfuck_bitwidth_conversions
doubleCellSize :: [Brainfuck] -> [Brainfuck]
doubleCellSize [] = []
doubleCellSize (BfLoopBegin : BfDec : BfLoopEnd : BfRead : insts) =
    bfFromString ">>>[-]<<<[-], " ++ doubleCellSize insts
doubleCellSize (inst : insts) = doubleCellSize' inst ++ doubleCellSize insts
    where
        doubleCellSize' :: Brainfuck -> [Brainfuck]
        doubleCellSize' BfMoveRight = bfFromString ">>>>"
        doubleCellSize' BfMoveLeft = bfFromString "<<<<"
        doubleCellSize' BfInc = bfFromString ">+<+[>-]>[->>+<]<<"
        doubleCellSize' BfDec = bfFromString ">+<[>-]>[->>-<]<<-"
        doubleCellSize' BfLoopBegin = bfFromString ">+<[>-]>[->+>[<-]<[<]>[-<+>]]<-[+<"
        doubleCellSize' BfLoopEnd = bfFromString ">+<[>-]>[->+>[<-]<[<]>[-<+>]]<-]<"
        doubleCellSize' BfWrite = [BfWrite]
        doubleCellSize' BfRead = error "Could not convert raw ',', expected '[-],'"
