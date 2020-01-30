module NewNewTypeChecker where

import Control.Monad.Except

--
-- https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
-- A tutorial implementation of a dependently typed lambda calculus
--

--
-- The basics for terms?
--

data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)

data InferableTerm
  = Ann CheckableTerm Type
  | Bound Int
  | Free Name
  | InferableTerm :@: CheckableTerm -- infix constructor :@:
                                    -- denotes application?
  deriving (Show, Eq)

data CheckableTerm
  = Inf InferableTerm
  | Lam CheckableTerm
  deriving (Show, Eq)

data Type
  = TFree Name
  | Fun Type Type
  deriving (Show, Eq)

data Value
  = VLam (Value -> Value)
  | VNeutral Neutral


data Neutral
  = NFree Name
  | NApp Neutral Value

-- const, when evaluated, results in
-- const = VLam (λx -> VLam (λy -> x))

vfree :: Name -> Value
vfree n = VNeutral (NFree n)


--
-- Evaluation
--

type Env = [Value]

evalInferable :: InferableTerm -> Env -> Value
evalInferable term env =
  case term of
    (Ann e _)  -> evalCheckable e env
    (Free x)   -> vfree x
    (Bound i)  -> env !! i
    (e :@: e') -> vapp (evalInferable e env) (evalCheckable e' env)

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

evalCheckable :: CheckableTerm -> Env -> Value
evalCheckable term env =
  case term of
    (Inf i) -> evalInferable i env
    (Lam e) -> VLam (\x -> evalCheckable e (x : env))

--
-- Context for typechecking
--

data Kind = Star
          deriving Show

data Info
  = HasKind Kind
  | HasType Type
  deriving Show

type Context = [(Name, Info)]

--
-- Typechecking
--

type Result a = Either String a

kindCheckable :: Context -> Type -> Kind -> Result ()
kindCheckable context (TFree x) Star =
  case lookup x context of
    Just (HasKind Star) -> return ()
    Nothing -> throwError "unknown identifier"

kindCheckable context (Fun k k') Star =
  do
    kindCheckable context k Star
    kindCheckable context k' Star

typeInfer0 :: Context -> InferableTerm -> Result Type
typeInfer0 context term = typeInfer 0 context term

typeInfer :: Int -> Context -> InferableTerm -> Result Type
typeInfer i context (Ann checkableTerm type') =
  do
    kindCheckable context type' Star
    typeCheckable i context checkableTerm type'
    return type'

typeInfer i context (Free x) =
  case lookup x context of
    Just (HasType t) -> return t
    Nothing -> throwError "unknown identifier"

typeInfer i context (e :@: e') =
  do
    omega <- typeInfer i context e
    case omega of
      Fun t t' -> do
        typeCheckable i context e' t
        return t'
      _ -> throwError "illegal application"

typeCheckable :: Int -> Context -> CheckableTerm -> Type -> Result ()
typeCheckable i context (Inf e) t =
  do
    t' <- typeInfer i context e
    unless (t == t') (throwError "type mismatch")
typeCheckable i context (Lam e) (Fun t t') =
  typeCheckable (i + 1) ((Local i, HasType t) : context)
                (substCheck 0 (Free (Local i)) e) t'
typeCheckable i context _ _ =
  throwError "type mistmatch"

substInfer :: Int -> InferableTerm -> InferableTerm -> InferableTerm
substInfer i r (Ann e t)  = Ann (substCheck i r e) t
substInfer i r (Bound j)  = if i == j then r else Bound j
substInfer i r (Free y)   = Free y
substInfer i r (e :@: e') = substInfer i r e :@: substCheck i r e'

substCheck :: Int -> InferableTerm -> CheckableTerm -> CheckableTerm
substCheck i r (Inf e) = Inf (substInfer i r e)
substCheck i r (Lam e) = Lam (substCheck (i + 1) r e)

--
-- Quoting (to print Funs)
--

quote0 :: Value -> CheckableTerm
quote0 = quote 0

quote :: Int -> Value -> CheckableTerm
quote i (VLam f)     = Lam (quote (i + 1) (f (vfree (Quote i))))
quote i (VNeutral n) = Inf (neutralQuote i n)

neutralQuote :: Int -> Neutral -> InferableTerm
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

boundfree :: Int -> Name -> InferableTerm
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i x         = Free x

--
-- Examples
--

id'     = Lam (Inf (Bound 0))
const'  = Lam (Lam (Inf (Bound 1)))
tfree a = TFree (Global a)
free x  = Inf (Free (Global x))

term1= Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y"

term2= Ann const' (Fun
                   (Fun (tfree "b") (tfree "b"))
                   (Fun (tfree "a") (Fun (tfree "b") (tfree "b"))))
  :@: id' :@: free "y"

env1 = [ (Global "y", HasType (tfree "a"))
       , (Global "a", HasKind Star) ]
env2 = [ (Global "b", HasKind Star) ] <> env1

-- quote0 (evalInferable term1 [])
-- > Inf (Free (Global "y"))
-- quote0 (evalInferable term2 [])
-- > Lam (Inf (Bound 0))
-- typeInfer0 env1 term1
-- > Right (TFree (Global "a"))
-- typeInfer0 env2 term2
-- > Right (Fun (TFree (Global "b")) (TFree (Global "b")))
