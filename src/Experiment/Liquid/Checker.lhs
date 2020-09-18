> module Experiment.Liquid.Checker where

We're going to walk through the steps for liquid types,
as proposed by:

http://goto.ucsd.edu/~rjhala/liquid/liquid_types.pdf

This particular example should infer, for max, that
the type is constrained to never lower than the lower
integer.

Our existing hindley-milner inference:

> import TypeChecker

Our existing language, only starting with the subset of

data Expr a
  = ENum Int
  | EVar String
  | EAp (Expr a) (Expr a)

> import Language
> import Parser

Just a helper to take us from something like:

"main = max"

to:

[("main", [], EVar "max")]

> syntax' :: String -> Language.CoreProgram
> syntax' = syntax . clex 0 0

1.

Here is our starting point from the paper:
run HM inference -- it doesn't really do anything besides
assign the type of max from the type environment to the
supercombinator max

> inferredMax = runTypeCheck
>   (syntax' "max = max")
>   [("max", Scheme [] (arrow int (arrow int int)))]

Running that gives us

[("max",
  Scheme []
  (TypeConstructor "arrow"
   [TypeConstructor "int" []
   ,TypeConstructor "arrow"
    [TypeConstructor "int" []
    ,TypeConstructor "int" []]]))]

Which if you squint a bit can be read as
max :: int -> int -> int

next up:

""Using this type, we create a template for the
  liquid type of max, x:κx→y:κy→κ1, where κx, κy, κ1
  are liquid type variables representing the unknown
  refinements for the formals x, y and the body of max""

since I don't know how this will work out, let's try

> type LiquidTypeVars = [Int]
>
> collectNonArrows :: TypeExpression -> [String]
> collectNonArrows (TypeConstructor "arrow" rest) =
>   [] <> (concat $ fmap collectNonArrows rest)
> collectNonArrows (TypeConstructor name rest) =
>   [name]
>
> toTemplate :: (String, TypeScheme) -> (String, LiquidTypeVars)
> toTemplate (name, (Scheme typeVars expr)) =
>   (name, (numberizeCollected $ collectNonArrows expr))
>   where
>     numberizeCollected n = [0..(length n - 1)]
>
> liquidTypeVars :: [(String, LiquidTypeVars)]
> liquidTypeVars = fmap toTemplate inferredMax

now:

liquidTypeVars = [("max", [0, 1, 2])]

alright, so our "template" is just a unique list of ints counting up.
so now kx = 0, (variable x)
       ky = 1, (variable y)
       k1 = 2  (body of max)

on to step 2!

""As the body is an if expression, our algorithm generates the
  following two constraints that stipulate that, under the
  appropriate branch condition, the then and else expressions,
  respectively x, y, have types that are subtypes of the entire
  body’s type""

So, constraint generation.

Here is roughly their algorithm (focusing on if then else first)

constraints :: Type -> Expression ->
constraints typ expression = case expression of
  EIf cond exprA exprB ->
    let
      f = fresh (hm (shape typ) expression)
      (_, condConstraint) = constraints typ cond
      (f2, exprAConstraint) = constraints (typ cond) exprA
      (f3, exprBConstraint) = constraints (typ cond) exprB
      -- f, c1 \/ c2 \/ c3 \/ { typ |- F}
      --   \/ {typ; cond |- f2 <: f }
      --   \/ {typ; cond |- f3 <: f}
