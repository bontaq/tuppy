module Language where

import Data.List (intersperse)

type Name = String
type IsRec = Bool

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

data Expr a
  = EVar Name -- Variables
  | ENum Int  -- Numbers
  | EConstr Int Int -- Constructor tag arity
  | EAp (Expr a) (Expr a) -- Applications
  | ELet                  -- Let (rec) expressions
    IsRec                 -- boolean with True = recursive
    [(a, Expr a)]         -- definitions
    (Expr a)              -- body of let(rec)
  | ECase
    (Expr a)
    [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)

type CoreExpr = Expr Name

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
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type Program a = [ScDefn a]
type CoreProgram = Program Name

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
        (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

newline :: [Char]
newline = "\n"

prettyPrintDefn :: (Name, CoreExpr) -> Iseq
prettyPrintDefn (name, expr) =
  iConcat [ iStr name, iStr " = ", prettyPrintExpr expr ]

prettyPrintDefns :: [(Name, CoreExpr)] -> Iseq
prettyPrintDefns defns =
  iInterleave sep (map prettyPrintDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

prettyPrintExpr :: CoreExpr -> Iseq

-- prettyPrintExpr (ENum n) = IStr n

prettyPrintExpr (EVar v) = IStr v

prettyPrintExpr (EAp e1 e2) =
  prettyPrintExpr e1
  `iAppend` IStr " "
  `iAppend` prettyPrintExpr e2

prettyPrintExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr " ", iIndent (prettyPrintDefns defns), iNewline
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
  iConcat [ iStr name
          , iInterleave (iStr " " ) $ map iStr args, iNewline
          , prettyPrintExpr exp, iNewline ]

prettyPrintProgram :: CoreProgram -> Iseq
prettyPrintProgram scs = iConcat $ map prettyPrintSC scs

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr = IStr

iAppend :: Iseq -> Iseq -> Iseq
iAppend seq1 seq2 = IAppend seq1 seq2

iIndent :: Iseq -> Iseq
iIndent seq = seq

iNewline :: Iseq
iNewline = IStr "\n"

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave spacer = foldr (iAppend . iAppend spacer) iNil

flatten :: [Iseq] -> String
flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ (flatten seqs)
flatten (IAppend s1 s2 : ss) = flatten (s1 : s2 : ss)

iDisplay :: Iseq -> String
iDisplay seq = flatten [seq]

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where e2s = repeat e2

pprint :: CoreProgram -> String
pprint prog = iDisplay (prettyPrintProgram prog)

-- pprint prog = prettyPrintProgram prog
