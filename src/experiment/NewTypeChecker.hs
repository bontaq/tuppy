module NewTypeChecker where

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Control.Monad        (replicateM)
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Except

import Language

----
-- Pretty much copying down http://dev.stephendiehl.com/fun/006_hindley_milner.html
----

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
     = TVar TVar
     | TCon String
     | TArr Type Type
     deriving (Show, Eq, Ord)

typeBool, typeInt :: Type
typeInt = TCon "int"
typeBool = TCon "bool"

data Scheme = Forall [TVar] Type

type Var = String

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (var, scheme) = TypeEnv $ Map.insert var scheme env

data TypeError
  = InfiniteType TVar Type
  | UnboundVariable String

data Unique = Unique { count :: Int }
type Infer a = ExceptT TypeError (State Unique) a

-- runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
-- runInfer m = case evalState (runExceptT m) initUnique of
--   Left err -> Left err
--   Right typeScheme -> Right $ closeOver typeScheme

type Subst = Map.Map TVar Type

class Substituable a where
  apply :: Subst -> a -> a
  freshTypeVar :: a -> Set.Set TVar

instance Substituable Type where
  apply _ (TCon a)           = TCon a
  apply subst t@(TVar a)     = Map.findWithDefault t a subst
  apply subst (t1 `TArr` t2) = apply subst t1 `TArr` apply subst t2

  freshTypeVar TCon{}       = Set.empty
  freshTypeVar (TVar a)     = Set.singleton a
  freshTypeVar (TArr t1 t2) = freshTypeVar t1 `Set.union` freshTypeVar t2

instance Substituable a => Substituable [a] where
  apply = fmap . apply
  freshTypeVar = foldr (Set.union . freshTypeVar) Set.empty

instance Substituable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where s' = foldr Map.delete s as

  freshTypeVar (Forall as t) =
    freshTypeVar t `Set.difference` Set.fromList as

instance Substituable TypeEnv where
  freshTypeVar (TypeEnv env) = freshTypeVar $ Map.elems env

compose :: Subst -> Subst -> Subst
compose subst1 subst2 = Map.map (apply subst1) subst2 `Map.union` subst1

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  pure $ TVar $ TV (letters !! count s)

occursCheck :: Substituable a => TVar -> a -> Bool
occursCheck tvar t = tvar `Set.member` freshTypeVar t

nullSubst :: Subst
nullSubst = Map.empty

unify :: Type -> Type -> Infer Subst
unify (TArr l r) (TArr l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  pure (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t

bind :: TVar -> Type -> Infer Subst
bind a t | t == TVar a = pure nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = pure $ Map.singleton a t

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  pure $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $
      freshTypeVar t `Set.difference` freshTypeVar env

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  ->
      do
        t <- instantiate s
        pure (nullSubst, t)

handleLetDef :: TypeEnv -> (Name, Expr Name) -> Infer TypeEnv
handleLetDef env (name, expr) = do
  (s1, t1) <- infer env expr
  let env' = apply s1 env
      t'   = generalize env' t1
  pure $ env' `extend` (name, t')

handleLetDefs :: TypeEnv -> [(Name, Expr Name)] -> Infer TypeEnv
handleLetDefs env defs =
  foldM (\env' expr -> handleLetDef env' expr) env defs

infer :: TypeEnv -> Expr String -> Infer (Subst, Type)
infer env expr = case expr of
  EVar x -> lookupEnv env x

  EAp e1 e2 -> do
    typeVar <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArr t2 typeVar)
    pure (s3 `compose` s2 `compose` s1, apply s3 typeVar)

  ELet isRecursive defs expr -> do
    -- extend env for each defn
    -- (s1, t1) <- infer env defs
    -- let -- env' = apply s1 env
        -- t'   = generalize env' t1
    env'' <- handleLetDefs env defs
    -- (s2, t2) <- infer (env' `extend` (x, t')) expr
    (s2, t2) <- infer env'' expr
    pure (s1 `compose` s2, t2)
