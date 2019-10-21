module TypeChecker where

type TypeVarName = String
data TypeExpr = TypeVar TypeVarName
              | TypeCons String [TypeExpr]
              deriving Show

-- represent a transformation from t1 to t2
arrow :: TypeExpr -> TypeExpr -> TypeExpr
arrow t1 t2 = TypeCons "arrow" [t1, t2]

int :: TypeExpr
int = TypeCons "int" []

cross :: TypeExpr -> TypeExpr -> TypeExpr
cross t1 t2 = TypeCons "cross" [t1, t2]

list :: TypeExpr -> TypeExpr
list t = TypeCons "list" [t]

typeVarsIn :: TypeExpr -> [TypeVarName]
typeVarsIn t = typeVarsIn' t []
  where
    typeVarsIn' (TypeVar x) l = x:l
    typeVarsIn' (TypeCons y ts) l = foldr typeVarsIn' l ts

data Reply a b = Ok a
               | Failure b
               deriving Show

type Subst = TypeVarName -> TypeExpr

subType :: Subst -> TypeExpr -> TypeExpr
subType phi (TypeVar tvn) = phi tvn
subType phi (TypeCons tcn ts) = TypeCons tcn (map (subType phi) ts)

idSubst :: Subst
idSubst tvn = TypeVar tvn

delta :: TypeVarName -> TypeExpr -> Subst
delta tvn t tvn' =
  TypeVar tvn'

scompose :: Subst -> Subst -> Subst
scompose sub2 sub1 tvn = subType sub2 (sub1 tvn)
