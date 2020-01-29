module NewNewTypeChecker where

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
