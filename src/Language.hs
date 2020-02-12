{-# LANGUAGE OverloadedStrings #-}

module Language where

type Name = String
type IsRec = Bool

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

data Expr a
  = EVar Name             -- Variables
  | ENum Int              -- Numbers
  | EStr String           -- Strings
  | EAp (Expr a) (Expr a) -- Application
  | ELam [a] (Expr a)     -- lambda expressions like \x -> x
  | ELet                  -- Let (rec) expressions
    IsRec                 ---- boolean with True = recursive
    [(a, Expr a)]         ---- definitions
    (Expr a)              ---- body of let(rec)

  -- TODO: | Ann (Expr a) Type
  | Ann a Type
  -- not supported yet
  -- | EConstr Int Int -- Constructor tag arity
  -- | ECase
  --   (Expr a)
  --   [Alter a]
  deriving (Show, Eq)

data Type
  = TFree Name
  | Fun Type Type
  deriving (Show, Eq)


type CoreExpr = Expr Name

-- supercombinator definition (i.e. main = )
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type Program a = [ScDefn a]
type CoreProgram = Program Name

-- example of the core language
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
