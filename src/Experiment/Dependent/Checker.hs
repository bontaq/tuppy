module Experiment.Dependent.Checker where

import Control.Monad.Except

--
-- https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
-- A tutorial implementation of a dependently typed lambda calculus
--

-- https://github.com/Ralith/tisp/blob/475a1d22ba37939328b2fe51faa49422f6981b95/src/Tisp/Value.hs#L28
-- https://github.com/chameco/bramble/blob/master/src/Bramble/Core/Calculus.hs

-- https://www.andres-loeh.de/LambdaPi/
-- https://www.andres-loeh.de/LambdaPi/LambdaPi-README

--
-- The basics for terms?
--

data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)

data InferableTerm
  = Ann CheckableTerm CheckableTerm
  | Star
  | Pi CheckableTerm CheckableTerm
  | Bound Int
  | Free Name
  | LitText String
  | InferableTerm :@: CheckableTerm -- infix constructor :@:
                                    -- denotes application?
  deriving (Show, Eq)

data CheckableTerm
  = Inf InferableTerm
  | Lam CheckableTerm
  deriving (Show, Eq)

type Type = Value
type Context = [(Name, Type)]

-- data Literal = LitNum String
--              | LitText String
--              deriving Show

data Value
  = VLam (Value -> Value)
  | VStar
  | VPi Value (Value -> Value)
  | VLit
  | VNeutral Neutral

instance Show Value where
  show = show . quote0

instance Eq Value where
  a == b = (show a) == (show b)

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
    (Star)     -> VStar
    (Pi t t')  -> VPi (evalCheckable t env) (\x -> evalCheckable t' (x : env))
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

-- data Kind = Star
--           deriving Show

-- data Info
--   = HasKind Kind
--   | HasType Type
--   deriving Show

-- type Context = [(Name, Info)]

--
-- Typechecking
--

type Result a = Either String a

typeInfer0 :: Context -> InferableTerm -> Result Type
typeInfer0 context term = typeInfer 0 context term

typeInfer :: Int -> Context -> InferableTerm -> Result Type
typeInfer i context (Ann checkableTerm p) =
  do
    typeCheckable i context p VStar
    let type' = evalCheckable p []
    typeCheckable i context checkableTerm type'
    return type'

typeInfer i context Star = return VStar

typeInfer i context (Pi p p') =
  do
    typeCheckable i context p VStar
    let type' = evalCheckable p []
    typeCheckable
      (i + 1)
      ((Local i, type') : context)
      (substCheck 0 (Free (Local i)) p') VStar
    return VStar

typeInfer i context (Free x) =
  case lookup x context of
    Just t -> return t
    Nothing -> throwError $ "unknown identifier " <> show x

typeInfer i context (LitText _) = return VLit

typeInfer i context (e :@: e') =
  do
    omega <- typeInfer i context e
    case omega of
      VPi t t' -> do
        typeCheckable i context e' t
        return (t' (evalCheckable e' []))
      _ -> throwError "illegal application"

typeCheckable :: Int -> Context -> CheckableTerm -> Type -> Result ()
typeCheckable i context (Inf e) v =
  do
    v' <- typeInfer i context e
    unless (quote0 v == quote0 v')
      (throwError $ "type mismatch: "
       <> (show $ quote0 v)
       <> " "
       <> (show $ quote0 v'))

typeCheckable i context (Lam e) (VPi t t') =
  typeCheckable
    (i + 1)
    ((Local i, t) : context)
    (substCheck 0 (Free (Local i)) e) (t' (vfree (Local i)))

typeCheckable i context _ _ =
  throwError "type mistmatch 2"

substInfer :: Int -> InferableTerm -> InferableTerm -> InferableTerm
substInfer i r (Ann e t)  = Ann (substCheck i r e) t
substInfer i r (Bound j)  = if i == j then r else Bound j
substInfer i r (Free y)   = Free y
substInfer i r Star       = Star
substInfer i r (Pi t t')  = Pi (substCheck i r t) (substCheck (i + 1) r t')
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
quote i VStar        = Inf Star
quote i (VPi v f)    = Inf (Pi (quote i v) (quote (i + 1) (f (vfree (Quote i)))))
quote i VLit         = Inf (LitText "string")

neutralQuote :: Int -> Neutral -> InferableTerm
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

boundfree :: Int -> Name -> InferableTerm
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i x         = Free x

--
-- Examples
--

id'     = Lam (Lam (Inf (Bound 0)))
const'  = Lam (Lam (Inf (Bound 1)))
tfree a = Free (Global a)
free x  = Inf (Free (Global x))

term1 = Ann id' (Inf $ Pi (free "a") (free "a")) -- :@: free "y"

term2 = Ann const' (Inf $ Pi
                    (Inf $ Pi (free "b") (free "b"))
                    (Inf $ Pi (free "a") (Inf $ Pi (free "b") (free "b"))))
  :@: id' :@: free "y"

env1 :: Context
env1 = [ (Global "y", VNeutral (NFree $ Global "a"))
       , (Global "a", VStar) ]
env2 :: Context
env2 = [ (Global "b", VStar) ] <> env1

-- quote0 (evalInferable term1 [])
-- > Inf (Free (Global "y"))
-- quote0 (evalInferable term2 [])
-- > Lam (Inf (Bound 0))

-- typeInfer0 env1 term1
-- > Right (TFree (Global "a"))
-- typeInfer0 env2 term2
-- > Right (Fun (TFree (Global "b")) (TFree (Global "b")))
