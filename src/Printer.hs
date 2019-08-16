{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.List (intersperse)
import Language

-- type Name = String
-- type IsRec = Bool

-- type Alter a = (Int, [a], Expr a)
-- type CoreAlt = Alter Name

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

-- data Expr a
--   = EVar Name -- Variables
--   | ENum Int  -- Numbers
--   | EConstr Int Int -- Constructor tag arity
--   | EAp (Expr a) (Expr a) -- Applications
--   | ELet                  -- Let (rec) expressions
--     IsRec                 -- boolean with True = recursive
--     [(a, Expr a)]         -- definitions
--     (Expr a)              -- body of let(rec)
--   | ECase
--     (Expr a)
--     [Alter a]
--   | ELam [a] (Expr a)
--   deriving (Show)

-- type CoreExpr = Expr Name

-- collects the list of variables
bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

-- collects the list of definitions
rightHandSideOf :: [(a, b)] -> [b]
rightHandSideOf defns = [rhs | (name, rhs) <- defns]

-- atomic expressions have no internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr _        = False

-- supercombinator definition (i.e. main = )
-- type ScDefn a = (Name, [a], Expr a)
-- type CoreScDefn = ScDefn Name

-- type Program a = [ScDefn a]
-- type CoreProgram = Program Name

-- defines some combinators for the core languages
-- mostly just function application
preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x")
    , ("K", ["x", "y"], EVar "x")
    , ("K1", ["x", "y"], EVar "y")
    , ("S"
      , ["f", "g", "x"]
      , EAp
        (EAp (EVar "f") (EVar "x"))
        (EAp (EVar "g") (EVar "x")))
    , ("compose"
      , ["f", "g", "x"]
      , EAp
        (EVar "f")
        (EAp (EVar "g") (EVar "x")))
    , ("twice"
      , ["f"]
      , EAp
        (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    , ("lettest"
      , ["f"]
      , ELet False
        [("f", (EAp (EVar "g") (EVar "x")))]
        (EAp (EVar "test") (EVar "f"))) ]

prettyPrintDefn :: (Name, CoreExpr) -> Iseq
prettyPrintDefn (name, expr) =
  iConcat [ iStr name, iStr " = ", iIndent $ prettyPrintExpr expr ]

prettyPrintDefns :: [(Name, CoreExpr)] -> Iseq
prettyPrintDefns defns =
  iInterleave sep (map prettyPrintDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

prettyPrintExpr :: CoreExpr -> Iseq

prettyPrintExpr (EVar v) = IStr v

prettyPrintExpr (EAp e1 e2) =
  prettyPrintExpr e1
  `iAppend` IStr " "
  `iAppend` prettyPrintExpr e2

prettyPrintExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iIndent iNewline
            , (prettyPrintDefns defns)
            , iStr "in ", prettyPrintExpr expr]
  where
    keyword | not isrec = "let"
            | isrec     = "letrec"

prettyPrintExpr (ECase expr alter)
  = iNil

prettyPrintExpr (ELam a expr)
  = iNil

-- places parenthese around expr unless it's atomic
prettyPrintAExpr :: CoreExpr -> Iseq
prettyPrintAExpr e
  | isAtomicExpr e = prettyPrintExpr e
  | otherwise      = iConcat [iStr "(", prettyPrintExpr e, iStr ")"]

prettyPrintSC :: ScDefn Name -> Iseq
prettyPrintSC (name, args, exp) =
  iConcat [ iStr name, iStr " "
          , iInterleave (iStr " " ) $ map iStr args, iStr "= "
          , prettyPrintExpr exp ]

prettyPrintProgram :: [CoreScDefn] -> Iseq
prettyPrintProgram scs =
  iInterleave sep (map prettyPrintSC scs)
  where
    sep = iConcat [iStr ";", iNewline]

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr str  = IStr str

iAppend :: Iseq -> Iseq -> Iseq
iAppend seq1 seq2 = IAppend seq1 seq2

iIndent :: Iseq -> Iseq
iIndent seq = IIndent seq

iNewline :: Iseq
iNewline = INewline

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave ins (i:is) = (i `iAppend` ins) `iAppend` iInterleave ins is

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n =
  iStr (space (width - length digits) ++ digits)
  where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map layItem (zip [1..] seqs))
  where
    layItem (n, seq)
      = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]

space :: Int -> String
space n = take n $ repeat ' '

flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten col ((INewline, indent) : seqs) =
  '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, _) : seqs) =
  flatten col ((seq, col + 2) : seqs)
flatten col ((IAppend s1 s2, indent) : ss) =
  flatten col ((s1, indent) : (s2, indent) : ss)
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IStr s, _) : seqs) = s ++ (flatten col seqs)

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where e2s = repeat e2

pprint :: CoreProgram -> String
pprint prog = iDisplay (prettyPrintProgram prog)
