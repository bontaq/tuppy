> module Constraint.CheckerLog where

> import Parser
> import Language
>
> import Data.List
> import Data.Maybe
> import Test.Hspec

Time to go insane

Query -> Facts -> Facts

data Maybe a = Just a | Nothing

instance Functor Maybe where

[:find ?e
 :where
 ?e :person/name "Ridley Scott"]

query [Nothing, Just a, Just b] =
  filter (\[eid, attr, value] -> attr == a && value == b)
  |> fmap (\[eid, attr, value] -> eid)

[const True, == "a", == "b"]
[Nothing, "a", "b"]
[Query "e", Rule == "a", Rule == "b"]

> apply :: (a -> b) -> a -> b
> apply f x = f x

So what rules would that put into the system?

x (no info)
f (only that it's a function)

> [(_, _, x)] = syntax $ clex 0 0 "apply f x = f x"

[
 ("apply", [],
   ELam ["f"] (ELam ["x"] (EAp (EVar "f") (EVar "x"))))
]

Where we're going, we'll need debruijn indexes

The above statement, as de bruijn indexes, would look like:
λ 2 1 -- f is bound 2 up, x is bound 1

Let's use something simple and dumb to represent indexes:
[["a", 1]] -- [[variable, index]]

Do we need to pass in a lookup?

debruijn :: Lookup -> Index -> Expr -> [[]] ?

[var, [var, [var]]] representation
  3     2     1     levels

Or is that being a bit too cute

> debruijn
>   :: Integer
>   -> [([String], Integer)]
>   -> Expr Name
>   -> Expr Integer
> debruijn level lookup (ELam var expr) =
>   debruijn (level + 1) (lookup <> [(var, level)]) expr
> debruijn level lookup (EAp a b) =
>   let
>     leftSide = debruijn level lookup a
>     rightSide = debruijn level lookup b
>   in
>     EAp leftSide rightSide
> debruijn level lookup (EVar x) =
>   let
>     found = find (\([var], level) -> var == x) lookup
>   in
>     EVar ((-) level $ snd $ fromJust found)

> test1 = hspec $ do
>   describe "debruijn" $ do
>
>     it "works for apply" $ do
>       let [(_, _, term)] = syntax $ clex 0 0 "apply f x = f x"
>       debruijn 0 [] term
>       `shouldBe`
>       EAp (EVar 2) (EVar 1)
>
>     it "works for const" $ do
>       let [(_, _, term)] = syntax $ clex 0 0 "const y x = y"
>       debruijn 0 [] term
>       `shouldBe`
>       EVar 2
