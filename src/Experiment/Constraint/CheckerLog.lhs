> module Constraint.CheckerLog where

> import Parser
> import Language
>
> import Data.List
> import Data.Maybe
> import Test.Hspec
>
> import GHC.Generics hiding (from)
> import Data.Hashable
> import Data.Holmes

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

> data Lambda = Lambda Lambda Lambda
>             | Val Integer
>             | Intro Integer
>             | Nil
>             deriving (Eq, Show)

> debruijn
>   :: Integer
>   -> [([String], Integer)]
>   -> Expr Name
>   -> Lambda
> debruijn level lookup (ELam var expr) =
>   Lambda
>   Nil
>   (debruijn (level + 1) (lookup <> [(var, level)]) expr)
>
> debruijn level lookup (EAp a b) =
>   let
>     leftSide = debruijn level lookup a
>     rightSide = debruijn level lookup b
>   in
>     Lambda leftSide rightSide
>
> debruijn level lookup (EVar x) =
>   let
>     found = find (\([var], level) -> var == x) lookup
>   in
>     Val ((-) level $ snd $ fromJust found)

> test1 = hspec $ do
>   describe "debruijn" $ do
>
>     it "works for apply" $ do
>       let [(_, _, term)] = syntax $ clex 0 0 "apply f x = f x"
>       debruijn 0 [] term
>       `shouldBe`
>       Lambda Nil (Lambda Nil (Lambda (Val 2) (Val 1)))
>
>     it "works for const" $ do
>       let [(_, _, term)] = syntax $ clex 0 0 "const y x = y"
>       debruijn 0 [] term
>       `shouldBe`
>       Lambda Nil (Lambda Nil (Val 2))

So that works, but I don't love the representation

> depth :: Lambda -> Integer
> depth (Lambda Nil x) = 1 + depth x
> depth _ = 1

> relabel :: Lambda -> Lambda
> relabel (Lambda Nil x) = Lambda (Intro $ depth x) (relabel x)
> relabel x = x
>
> test2 = hspec $ do
>   describe "relabel" $ do
>     it "labels the number" $ do
>       let [(_, _, term)] = syntax $ clex 0 0 "const y x = y"
>           dbd = debruijn 0 [] term
>       relabel dbd
>       `shouldBe`
>       Lambda (Intro 2) (Lambda (Intro 1) (Val 2))
>     it "works for apply" $ do
>       let [(_, _, term)] = syntax $ clex 0 0 "apply f x = f x"
>           dbd = debruijn 0 [] term
>       relabel dbd
>       `shouldBe`
>       Lambda (Intro 2) (Lambda (Intro 1) (Lambda (Val 2) (Val 1)))

I think that's good enough for us to work with, let's try the
constraint solver

> definedConfig :: Config Holmes (Intersect Char)
> definedConfig = 1 `from` [ 'a' .. 'd' ]

So we know Lambda (Val _) (Val _) means application

constraints - f :: Type(x) -> Anything
           or x :: f(Type(x))
     and retval :: Anything
constraints
[x, y]
x .== y

> data TypeDef = TypeApp String String

> matchConstraint x y = x .== y

think thinkthink

2 :: 1 -> 2
1 :: 1

do we even need debruijn?

let's just use the current language and improve it

or maybe I just set myself on fire?

I should go back to the prolog based approach

Ok, let's see if you we can use holmes.

entity attribute value

runRule (a, b, c) dataSet =
  let x = mempty
  in
    using dataSet

Fuck this, restarting

e a v

> data Attribute
>   = AType
>   | AName
>   deriving (Show, Eq)
>
> data Value
>   = VType Type
>   | VString String
>   deriving (Show, Eq)

> type Fact = (String, Attribute, Value)
> type DB = [(String, Attribute, Value)]
>
> runRule
>   :: (String -> Bool, Attribute -> Bool, Value -> Bool)
>   -> DB
>   -> DB
> runRule (a, b, c) db =
>   [(x, y, z) | (x, y, z) <- db
>              , a(x)
>              , b(y)
>              , c(z)]
>
> addFact :: (String, Attribute, Value) -> DB -> DB
> addFact = (:)

ok, now we have that (super)

now let's actually do something with it

> [program] = syntax $ clex 0 0 "num = 1"

> factize :: CoreScDefn -> DB -> DB
> factize (name, args, expr) db = factize' name expr db
>
> factize' :: String -> Expr Name -> DB -> [Fact]
> factize' name (ENum _) db =
>   [ (name, AName, VString name)
>   , (name, AType, VType $ TFree "Integer")
>   ]
> factize' name (EVar var) db =
>   getFactsByName var db
> factize' name (EAp a b) db =
>   let
>     typeA = getTypeByName "a" $ factize' "a" a db
>     typeB = getTypeByName "b" $ factize' "b" b db
>   in
>     undefined
>
> getFactsByName :: String -> DB -> DB
> getFactsByName name = filter (\(n, _, _) -> n == name)
>
> getTypeByName :: String -> DB -> Value
> getTypeByName name =
>   getType
>   . head
>   . filter (\fact@(n, _, _) -> n == name && isType fact)
>
> isType :: Fact -> Bool
> isType (_, AType, _) = True
> isType  _            = False
>
> getType :: Fact -> Value
> getType (_, AType, typ) = typ

plus :: Integer -> Integer -> Integer
plus 1 1

TFun "Integer" -> TFun "Integer" "Integer"

>
> mkProgram = head . syntax . (clex 0 0)
>

> test3 = hspec $ do
>   describe "factize" $ do
>     it "adds facts for ENum" $ do
>       factize (mkProgram "num = 1") []
>       `shouldBe`
>       [ ("num", AName, VString "num")
>       , ("num", AType, VType (TFree "Integer"))
>       ]
>     it "adds facts for plus" $ do
>       factize (mkProgram "y = plus 1 1")
>         [ ("plus", AName, VString "plus")
>         , ("plus", AType, VType (Fun
>                                  (TFree "Integer")
>                                  (Fun
>                                   (TFree "Integer")
>                                   (TFree "Integer"))))
>         ]
>       `shouldBe`
>       [ ("num", AName, VString "num")
>       , ("num", AType, VType (TFree "Integer"))
>       ]
