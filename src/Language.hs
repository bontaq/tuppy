{-# LANGUAGE OverloadedStrings #-}

module Language where

import Data.List (intersperse)

type Name = String
type IsRec = Bool

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- recursive, nonRecursive :: IsRec
-- recursive    = True
-- nonRecursive = False

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
-- bindersOf :: [(a, b)] -> [a]
-- bindersOf defns = [name | (name, rhs) <- defns]

-- collects the list of definitions
-- rightHandSideOf :: [(a, b)] -> [b]
-- rightHandSideOf defns = [rhs | (name, rhs) <- defns]

-- atomic expressions have no internal structure
-- isAtomicExpr :: Expr a -> Bool
-- isAtomicExpr (EVar v) = True
-- isAtomicExpr (ENum n) = True
-- isAtomicExpr _        = False

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
        (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    , ("lettest"
      , ["f"]
      , ELet False
        [("f", (EAp (EVar "g") (EVar "x")))]
        (EAp (EVar "test") (EVar "f"))) ]
