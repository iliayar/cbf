{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module TestImp where

import qualified BasicExt
import Executer (execute)
import Imp
import qualified SafeProc
import Test.HUnit
import qualified UncheckedInsts
import qualified UncheckedInstsExt
import qualified UncheckedProc

evaluate :: Program -> IO [Int]
evaluate program =
  execute $
    BasicExt.convert $
      UncheckedInsts.convert $
        UncheckedInstsExt.convert $
          UncheckedProc.convert $
            SafeProc.convert $
              Imp.convert program

re :: String -> Expr
re s = ExprRef $ RefVar $ Var s

kub :: Int -> Program
kub n =
  Program
    []
    [ (Function "main" [] TyVoid)
        [ StmtAllocate (Var "res") TyInt,
          StmtAssgn (RefVar $ Var "res") (ExprCall (Func "kub") [ExprConst n])
        ],
      (Function "mult" [("a", TyInt), ("b", TyInt)] TyInt)
        [ StmtAllocate (Var "res") TyInt,
          StmtAssgn (RefVar $ Var "res") (ExprConst 0),
          StmtWhile (re "a") 
            [ StmtAssgn (RefVar $ Var "res") (ExprAdd (re "res") (re "b"))
            , StmtAssgn (RefVar $ Var "a") (ExprSub (re "a") (ExprConst 1))
            ],
          StmtReturn $ Just $ re "res"
        ],
      (Function "kub" [("a", TyInt)] TyInt)
        [ StmtReturn $ Just $ ExprCall (Func "mult") [re "a", ExprCall (Func "mult") [re "a", re "a"]]
        ]
    ]

fact :: Int -> Program
fact n =
  Program
    []
    [ (Function "main" [] TyVoid)
        [ StmtAllocate (Var "res") TyInt,
          StmtAssgn (RefVar $ Var "res") (ExprCall (Func "fact") [ExprConst n])
        ],
      (Function "mult" [("a", TyInt), ("b", TyInt)] TyInt)
        [ StmtAllocate (Var "res") TyInt,
          StmtAssgn (RefVar $ Var "res") (ExprConst 0),
          StmtWhile (re "a") 
            [ StmtAssgn (RefVar $ Var "res") (ExprAdd (re "res") (re "b"))
            , StmtAssgn (RefVar $ Var "a") (ExprSub (re "a") (ExprConst 1))
            ],
          StmtReturn $ Just $ re "res"
        ],
      (Function "fact" [("a", TyInt)] TyInt)
        [ StmtIf (re "a")
            [ StmtReturn $ Just $ ExprCall (Func "mult") [re "a", ExprCall (Func "fact") [ExprSub (re "a") (ExprConst 1)]]
            ]
            [ StmtReturn $ Just $ ExprConst 1
            ]
        ]
    ]

testKub :: Int -> Int -> Test
testKub n res = TestCase $ do
  let prog = kub n
  mem <- evaluate prog
  (mem !! 4) @?= res

testFact :: Int -> Int -> Test
testFact n res = TestCase $ do
  let prog = fact n
  mem <- evaluate prog
  (mem !! 4) @?= res

sumArray :: [Int] -> Program
sumArray ns =
  let inits = zipWith (\i v -> StmtAssgn (RefArray (RefVar $ Var "arr") (ExprConst i)) (ExprConst v)) [0..] ns in 
  Program
    []
    [ (Function "main" [] TyVoid) $
        [ StmtAllocate (Var "res") TyInt,
          StmtAssgn (RefVar $ Var "res") (ExprConst 0),
          StmtAllocate (Var "arr") $ TyArray TyInt $ length ns
        ] ++ inits ++
        [ StmtAssgn (RefVar $ Var "arr") (ExprCall (Func "inc_all") [re "arr"]),
          StmtAssgn (RefVar $ Var "res") (ExprCall (Func "sum") [re "arr"])
        ],
      (Function "inc_all" [("arr", TyArray TyInt $ length ns)] (TyArray TyInt $ length ns))
        [ StmtAllocate (Var "i") TyInt,
          StmtAssgn (RefVar $ Var "i") (ExprConst $ length ns),
          StmtWhile (re "i")
            [ StmtAssgn (RefVar $ Var "i") (ExprSub (re "i") (ExprConst 1)),
              StmtAssgn (RefArray (RefVar $ Var "arr") (re "i")) (ExprAdd (ExprRef (RefArray (RefVar $ Var "arr") (re "i"))) (ExprConst 1))
            ],
          StmtReturn $ Just $ re "arr"
        ],
      (Function "sum" [("arr", TyArray TyInt $ length ns)] TyInt)
        [ StmtAllocate (Var "i") TyInt,
          StmtAllocate (Var "res") TyInt,
          StmtAssgn (RefVar $ Var "i") (ExprConst $ length ns),
          StmtAssgn (RefVar $ Var "res") (ExprConst 0),
          StmtWhile (re "i")
            [ StmtAssgn (RefVar $ Var "i") (ExprSub (re "i") (ExprConst 1)),
              StmtAssgn (RefVar $ Var "res") (ExprAdd (re "res") (ExprRef (RefArray (RefVar $ Var "arr") (re "i"))))
            ],
          StmtReturn $ Just $ re "res"
        ]
    ]



testSumArray :: [Int] -> Int -> Test
testSumArray arr res = TestCase $ do
  let prog = sumArray arr
  mem <- evaluate prog
  (mem !! 4) @?= res + length arr

structs :: Program
structs =
  Program
    [("S", TyStruct [("a", TyInt), ("b", TyArray TyInt 5), ("c", TyInt)])]
    [ (Function "main" [] TyVoid)
        [ StmtAllocate (Var "res") TyInt,
          StmtAssgn (RefVar $ Var "res") (ExprConst 42),
          StmtAllocate (Var "s") (TyAlias "S"),
          StmtAssgn (RefStructField (RefVar $ Var "s") "a") $ ExprConst 10,
          StmtAssgn (RefVar $ Var "res") (ExprRef $ RefStructField (RefVar $ Var "s") "a"),
          StmtAssgn (RefArray (RefStructField (RefVar $ Var "s") "b") (ExprConst 1)) $ ExprConst 20,
          StmtAssgn (RefArray (RefStructField (RefVar $ Var "s") "b") (ExprConst 0)) $ ExprConst 21,
          StmtAssgn (RefStructField (RefVar $ Var "s") "c") $ ExprConst 121,
          StmtAssgn (RefVar $ Var "s") (ExprCall (Func "modify_struct") [ExprRef $ RefVar $ Var "s"])
        ],
      (Function "modify_struct" [("s", (TyAlias "S"))] (TyAlias "S"))
        [ StmtAssgn (RefArray (RefStructField (RefVar $ Var "s") "b") (ExprConst 2)) $ ExprConst 19,
          StmtAssgn (RefStructField (RefVar $ Var "s") "a") $ ExprConst 74,
          StmtReturn $ Just $ ExprRef $ RefVar $ Var "s"
        ]
    ]

testStructs :: Test
testStructs = TestCase $ do
  let prog = structs
  mem <- evaluate prog
  (mem !! 5) @?= 10
  (mem !! 6) @?= 74
  (mem !! 11) @?= 21
  (mem !! 12) @?= 20
  (mem !! 13) @?= 19
  (mem !! 16) @?= 121

tests :: Test
tests =
  TestList
    [ testKub 11 51,
      testKub 2 8,
      testKub 7 87,
      testFact 0 1,
      testFact 1 1,
      testFact 4 24,
      testFact 7 176,
      testFact 9 128,
      testSumArray [1, 2, 3, 4, 5] 15,
      testSumArray [] 0,
      testSumArray [10] 10,
      testSumArray [10, 15, 33] 58,
      testStructs
    ]
