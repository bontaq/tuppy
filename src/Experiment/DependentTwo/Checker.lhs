> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> module DependentTwo.Checker where

Inferable and checkable terms are separated

Inferable terms means that our algorithm can produce the type
Checkable means that we need a type and then can check the term

> data TermInferable
>   = Annotated TermCheckable TermCheckable

New terms for dependent types

>   | Star
>   | Pi TermCheckable TermCheckable

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

This is "higher-order abstract syntax" to represent values
Values that are functions are represented using haskell functions
This is so that we don't have to implement substitution ourselves

For example, const, when evalutated, results in the Value:
LambdaValue (\x -> LambdaValue (\y -> x))

> data Value
>   = LambdaValue (Value -> Value)
>   | NeutralValue Neutral

Also new for dependent types, since types are values and values type

>   | StarValue
>   | PiValue Value (Value -> Value)

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
>   Star             -> StarValue
>   Pi t t'          -> PiValue (evalCheckable t env) (\x -> evalCheckable t' (x : env))
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

data Kind = Star
  deriving (Show)

data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

Extending a context is achieved by the list "cons" operation.  Lookup
is done via the haskell function lookup.  The context holds all our
types for the environment

Types are on the term level now, and we store the evaluated types

> type Type = Value
> type Context = [(Name, Type)]

This will model results for type checking

> type Result a = Either String a

The function for inferable terms, InferType, returns a type.  CheckType
returns Left or Right.  The well-formedness of types is done by CheckKind.

Type checking functions are parameterized by the number of binders we have
encountered.  On the initial call, this is 0.

checkKind :: Context -> Type -> Kind -> Result ()
checkKind env (TypeFree x) Star =
  case lookup x env of
    Just (HasKind Star) -> Right ()
    Nothing             -> Left "unknown identifier"
checkKind env (Function k k') Star = do
  checkKind env k Star
  checkKind env k' Star

> inferType :: Context -> TermInferable -> Result Type
> inferType = inferType' 0
>
> inferType' :: Int -> Context -> TermInferable -> Result Type
> inferType' i env (Annotated expr pi) = do
>   checkType i env pi StarValue
>   let typ = evalCheckable pi []
>   checkType i env expr typ
>   pure typ
> inferType' i env Star = Right StarValue
> inferType' i env (Pi p p') = do
>   checkType i env p StarValue
>   let t = evalCheckable p []
>   checkType (i + 1) ((Local i, t) : env)
>     (substCheckable 0 (Free (Local i)) p') StarValue
>   Right StarValue
> inferType' i env (Free x) =
>   case lookup x env of
>     Just t  -> Right t
>     Nothing -> Left "unknown identifier"
> inferType' i env (expr :@: expr') = do
>   omega <- inferType' i env expr
>   case omega of
>     PiValue t t' -> do checkType i env expr' t
>                        Right (t' (evalCheckable expr' []))
>     _             -> Left "illegal application"
>
> checkType :: Int -> Context -> TermCheckable -> Type -> Result ()
> checkType i env (Inferable expr) value = do
>   value' <- inferType' i env expr
>   if (quote value /= quote value') then
>     Left $ "type mismatch: " <> show (quote value) <> " " <> show (quote value')
>   else
>     Right ()

Because we are turning a bound variable into a free variable, we have
to perform substitution on the body.

> checkType i env (Lambda expr) (PiValue t t') =
>   checkType (i + 1) ((Local i, t) : env)
>             (substCheckable 0 (Free (Local i)) expr)
>             (t' (vfree (Local i)))


The integer argument indicates which variable is to be substituted

> substInferable :: Int -> TermInferable -> TermInferable -> TermInferable
> substInferable i r (Annotated expr t) = Annotated (substCheckable i r expr) t
> substInferable i r (Bound j) =
>   if i == j then r else Bound j
> substInferable i r (Free y) = Free y
> substInferable i r Star = Star
> substInferable i r (Pi t t') =
>   Pi (substCheckable i r t) (substCheckable (i + 1) r t')
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
> quote' i StarValue        = Inferable Star
> quote' i (PiValue v f)    = Inferable (Pi (quote' i v) (quote' (i + 1) (f (vfree (Quote i)))))

> neutralQuote :: Int -> Neutral -> TermInferable
> neutralQuote i (FreeNeutral x)    = boundfree i x
> neutralQuote i (ApplyNeutral n v) = neutralQuote i n :@: quote' i v

> boundfree :: Int -> Name -> TermInferable
> boundfree i (Quote k) = Bound (i - k - 1)
> boundfree i x         = Free x

> typeFree a = Free (Global a)

> free :: String -> TermCheckable
> free x = Inferable (Free (Global x))

> id' :: TermCheckable
> id' = Lambda (Lambda (Inferable (Bound 0))) -- why another lambda?

> termOne =
>   (Annotated id'
>     (Inferable (Pi (Inferable Star)
>                (Inferable
>                 (Pi
>                   (Inferable (Bound 0)) (Inferable (Bound 1))))))) -- :@: free "y"

> apTest =
>   termOne :@: free "Bool" :@: free "y"

apTest2 =
  termOne :@: (Inferable (Annotated
                          -- (Inferable (Free (Global "y")))
                          (Lambda (Inferable (Bound 0)))
                          (Inferable (Free (Global "Bool")))))

Right (Inferable (Pi (Inferable (Free (Global "Bool"))) (Inferable (Free (Global "Bool")))))

(Ann
  (Lam (Lam (Inf (Bound 0)))
  (Inf (Pi (Inf (Free (Global "a))) (Inf (Free (Global "a"))))

> envOne = [ (Global "y", (vfree (Global "Bool")))
>           , (Global "Bool", StarValue)
>           , (Global "a", StarValue)
>           , (Global "b", StarValue) ]
 
playing with the results from the reference implementation

let id = (\\a x -> x) :: forall (a :: *) . a -> a
(Let "id"
  (Ann_
    (Lam_ (Lam_ (Inf_ (Bound_ 0))))
    (Inf_ (Pi_ (Inf_ Star_) (Inf_ (Pi_ (Inf_ (Bound_ 0)) (Inf_ (Bound_ 1))))))
    ))

λ> parseIO "" (isparse lp) "let y = 1"
Ann_ (Succ_ Zero_) (Inf_ Nat_)

Annotated (Lambda (Lambda (Inferable (Bound 0))))
  (Inferable (Pi (Inferable Star) (Inferable (Pi (Inferable (Free (Global "a"))) (Inferable (Free (Global "a")))))))

let const = (\\a b x y -> x) :: forall (a :: *) (b :: *) . a -> b -> a
(Let "const" (Ann_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (Bound_ 1)))))) (Inf_ (Pi_ (Inf_ Star_) (Inf_ (Pi_ (Inf_ Star_) (Inf_ (Pi_ (Inf_ (Bound_ 1)) (Inf_ (Pi_ (Inf_ (Bound_ 1)) (Inf_ (Bound_ 3))))))))))))

Right (Inferable (Pi (Inferable Star)
                     (Inferable (Pi (Inferable (Bound 0))
                                    (Inferable (Bound 1))))))
