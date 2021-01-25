> module DependentTwo.Checker where

Inferable and checkable terms are separated

Inferable terms means that our algorithm can produce the type
Checkable means that we need a type and then can check the term

> data TermInferable
>   = Annotated TermCheckable Type

Locally bound variables are represent by de Bruijn indices

It represents how many binders occur between its binder and the occurance,
ie id can be represented as \ -> 0, and const as \ -> \ -> 1

>   | Bound Int

Free variables are represented by names, as in strings
Local free variables are represented by number
Global free variables are represented by string
This is called "locally nameless" representation

>   | Free Name

This represents application

>   | TermInferable :@: TermCheckable
>   deriving (Show, Eq)

> data TermCheckable

Inferable terms are embedded in checkable terms via Inferable
Lambda abstractions (which do introduce an explicit variable) are
introduced via Lambda

>   = Inferable TermInferable
>   | Lambda TermCheckable
>   deriving (Show, Eq)

> data Name
>   = Global String

When passing a binder in an algorithm, we have to convert a bound
variable into a free variable temporarily and use Local for that.

>   | Local Int
>   | Quote Int
>   deriving (Show, Eq)

TypeFree: TypeIdentifier
Function: An Arrow

Name is reused

In the simply typed lambda calculus, there are no bound names on
the type level, so no TypeBound constructor

> data Type
>   = TypeFree Name
>   | Function Type Type
>   deriving (Show, Eq)

This is "higher-order abstract syntax" to represent values
Values that are functions are represented using haskell functions
This is so that we don't have to implement substitution ourselves

For example, const, when evalutated, results in the Value:
LambdaValue (\x -> LambdaValue (\y -> x))

> data Value
>   = LambdaValue (Value -> Value)
>   | NeutralValue Neutral

Still not sure what a neutral term is

> data Neutral
>   = FreeNeutral Name
>   | ApplyNeutral Neutral Value

But now we have a function to create the value corresponding to
a free variable

> vfree :: Name -> Value
> vfree name = NeutralValue (FreeNeutral name)

> type Env = [Value]

> evalInferable :: TermInferable -> Env -> Value
> evalInferable term env = case term of
>   Annotated expr _ -> evalCheckable expr env
>   Free x           -> vfree x
>   Bound i          -> env !! i
>   e :@: e'         -> vapp (evalInferable e env) (evalCheckable e' env)

> vapp :: Value -> Value -> Value
> vapp (LambdaValue f) value  = f value
> vapp (NeutralValue neutral) value = NeutralValue (ApplyNeutral neutral value)

> evalCheckable :: TermCheckable -> Env -> Value
> evalCheckable term env = case term of
>   Inferable i -> evalInferable i env
>   Lambda e    -> LambdaValue (\x -> evalCheckable e (x : env))

Be we can implement type checking, we have to define contexts.
Context are implmented as (reversed) lists associating names with
either * (HasKind Star) or a type (HasType t)

> data Kind = Star
>   deriving (Show)

> data Info
>   = HasKind Kind
>   | HasType Type
>   deriving (Show)

Extending a context is achieved by the list "cons" operation.  Lookup
is done via the haskell function lookup.  The context holds all our
types for the environment

> type Context = [(Name, Info)]


This will model results for type checking

> type Result a = Either String a

The function for inferable terms, InferType, returns a type.  CheckType
returns Left or Right.  The well-formedness of types is done by CheckKind.

Type checking functions are parameterized by the number of binders we have
encountered.  On the initial call, this is 0.

> checkKind :: Context -> Type -> Kind -> Result ()
> checkKind env (TypeFree x) Star =
>   case lookup x env of
>     Just (HasKind Star) -> Right ()
>     Nothing             -> Left "unknown identifier"
> checkKind env (Function k k') Star = do
>   checkKind env k Star
>   checkKind env k' Star

> inferType :: Context -> TermInferable -> Result Type
> inferType = inferType' 0
>
> inferType' :: Int -> Context -> TermInferable -> Result Type
> inferType' i env (Annotated expr typ) = do
>   checkKind env typ Star
>   checkType i env expr typ
>   pure typ
> inferType' i env (Free x) =
>   case lookup x env of
>     Just (HasType t) -> Right t
>     Nothing          -> Left "unknown identifier"
> inferType' i env (expr :@: expr') = do
>   omega <- inferType' i env expr
>   case omega of
>     Function t t' -> do checkType i env expr' t
>                         Right t'
>     _             -> Left "illegal application"
>
> checkType :: Int -> Context -> TermCheckable -> Type -> Result ()
> checkType i env (Inferable expr) typ = do
>   typ' <- inferType' i env expr
>   if (typ /= typ') then
>     Left "type mismatch"
>   else
>     Right ()

Because we are turning a bound variable into a free variable, we have
to perform substitution on the body.

> checkType i env (Lambda expr) (Function t t') =
>   checkType (i + 1) ((Local i, HasType t) : env)
>             (substCheckable 0 (Free (Local i)) expr) t'

The integer argument indicates which variable is to be substituted

> substInferable :: Int -> TermInferable -> TermInferable -> TermInferable
> substInferable i r (Annotated expr t) = Annotated (substCheckable i r expr) t
> substInferable i r (Bound j) =
>   if i == j then r else Bound j
> substInferable i r (Free y) = Free y
> substInferable i r (expr :@: expr') =
>   substInferable i r expr :@: substCheckable i r expr'

> substCheckable :: Int -> TermInferable -> TermCheckable -> TermCheckable
> substCheckable i r (Inferable expr) = Inferable (substInferable i r expr)
> substCheckable i r (Lambda expr) = Lambda (substCheckable (i + 1) r expr)

quoting is needed to print the haskell functions, because normal haskell
functions are unprintable

> quote :: Value -> TermCheckable
> quote = quote' 0

> quote' :: Int -> Value -> TermCheckable
> quote' i (LambdaValue f)  = Lambda (quote' (i + 1) (f (vfree $ Quote i)))
> quote' i (NeutralValue n) = Inferable (neutralQuote i n)

> neutralQuote :: Int -> Neutral -> TermInferable
> neutralQuote i (FreeNeutral x)    = boundfree i x
> neutralQuote i (ApplyNeutral n v) = neutralQuote i n :@: quote' i v

> boundfree :: Int -> Name -> TermInferable
> boundfree i (Quote k) = Bound (i - k - 1)
> boundfree i x         = Free x

> typeFree a = TypeFree (Global a)
> free x = Inferable (Free (Global x))
> id' = Lambda (Inferable (Bound 0))
> termOne = (Annotated id' (Function (typeFree "a") (typeFree "a"))) :@: free "y"
> envOne = [ (Global "y", HasType (typeFree "a"))
>          , (Global "a", HasKind Star)
>          , (Global "b", HasKind Star) ]
