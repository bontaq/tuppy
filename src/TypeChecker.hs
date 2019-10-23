module TypeChecker where

import Language

type TypeVarName = [Integer]
type VExpr = Expr [Integer]
data TypeExpression = TypeVar TypeVarName
                    | TypeConstructor String [TypeExpression]
                    deriving (Show, Eq)

-- represent a transformation from t1 to t2
arrow :: TypeExpression -> TypeExpression -> TypeExpression
arrow t1 t2 = TypeConstructor "arrow" [t1, t2]

int :: TypeExpression
int = TypeConstructor "int" []

cross :: TypeExpression -> TypeExpression -> TypeExpression
cross t1 t2 = TypeConstructor "cross" [t1, t2]

list :: TypeExpression -> TypeExpression
list t = TypeConstructor "list" [t]

typeVarsIn :: TypeExpression -> [TypeVarName]
typeVarsIn t = typeVarsIn' t []
  where
    typeVarsIn' (TypeVar x) l = x:l
    typeVarsIn' (TypeConstructor y ts) l = foldr typeVarsIn' l ts

data Reply a b = Ok a
               | Failure b
               deriving Show

type Subst = TypeVarName -> TypeExpression

subType :: Subst -> TypeExpression -> TypeExpression
subType phi (TypeVar tvn) = phi tvn
subType phi (TypeConstructor tcn ts) = TypeConstructor tcn (map (subType phi) ts)

idSubstitution :: Subst
idSubstitution tvn = TypeVar tvn

delta :: TypeVarName -> TypeExpression -> Subst
delta tvn t tvn'
  | tvn == tvn' = t
  | otherwise   = TypeVar tvn'

scompose :: Subst -> Subst -> Subst
scompose sub2 sub1 tvn = subType sub2 (sub1 tvn)

extend :: Subst -> TypeVarName -> TypeExpression -> Reply Subst String
extend phi tvn t
  | t == TypeVar tvn        = Ok phi
  | tvn `elem` typeVarsIn t = Failure ""
  | otherwise =
      Ok ((delta tvn t) `scompose` phi)

unify :: Subst -> (TypeExpression, TypeExpression) -> Reply Subst String
unify phi ((TypeVar tvn), t)
  | phitvn == TypeVar tvn = extend phi tvn phit
  | otherwise = unify phi (phitvn, phit)
    where
      phitvn = phi tvn
      phit   = subType phi t

unify phi ((TypeConstructor tcn ts), (TypeVar tvn))
  = unify phi ((TypeVar tvn), (TypeConstructor tcn ts))

unify phi ((TypeConstructor tcn ts), (TypeConstructor tcn' ts'))
  | tcn == tcn' = unifyl phi (ts `zip` ts')
  | otherwise = Failure ""

unifyl :: Subst -> [(TypeExpression, TypeExpression)] -> Reply Subst String
unifyl phi eqns = foldr unify' (Ok phi) eqns
  where
    unify' eqn (Ok phi)    = unify phi eqn
    unify' eqn (Failure _) = Failure ""


data TypeScheme = Scheme [TypeVarName] TypeExpression

unknownScheme :: TypeScheme -> [TypeVarName]
unknownScheme (Scheme scvs t) =
  let tvars = typeVarsIn t
  in filter (\x -> not $ x `elem` scvs) tvars

subScheme :: Subst -> TypeScheme -> TypeScheme
subScheme phi (Scheme scvs t) =
  Scheme scvs (subType (exclude phi scvs) t)
  where
    exclude phi scvs tvn
      | tvn `elem` scvs = TypeVar tvn
      | otherwise       = phi tvn

type AssocList a b = [(a, b)]

dom :: AssocList a b -> [a]
dom al = [ k | (k, v) <- al ]

val :: Eq a => AssocList a b -> a -> b
val al k = head [ v | (k', v) <- al
                    , k == k' ]

install :: [(a, b)] -> a -> b -> [(a, b)]
install al k v = (k, v) : al

mg :: Eq a => AssocList a b -> [b]
mg al = map (val al) (dom al)

type TypeEnv = AssocList TypeVarName TypeScheme

unknownTypeEnv :: TypeEnv -> [TypeVarName]
unknownTypeEnv gamma = appendList (map unknownScheme (mg gamma))
  where
    appendList = foldr (++) []

subTypeEnv :: Subst -> TypeEnv -> TypeEnv
subTypeEnv phi gamma =
  [ (x, subScheme phi st) | (x, st) <- gamma ]

type NameSupply = TypeVarName

nextName ns = ns

deplete (n:ns) = (n + 2 : ns)

split ns = (0:ns, 1:ns)

nameSequence :: NameSupply -> [TypeVarName]
nameSequence ns = nextName ns : nameSequence (deplete ns)

typeCheck ::
  TypeEnv ->
  NameSupply ->
  VExpr ->
  Reply (Subst, TypeExpression) String
typeCheck gamma ns (EVar x) = typeCheckVar gamma ns x

typeCheckList ::
  TypeEnv
  -> NameSupply
  -> [VExpr]
  -> Reply (Subst, [TypeExpression]) String
typeCheckList gamma ns [] = Ok (idSubstitution, [])
typeCheckList gamma ns (e:es) =
  typeCheckList1 gamma ns0 es (typeCheck gamma ns1 e)
  where
    (ns0, ns1) = split ns

typeCheckList1 ::
  [(TypeVarName, TypeScheme)]
  -> NameSupply
  -> [VExpr]
  -> Reply (Subst, TypeExpression) String
  -> Reply (Subst, [TypeExpression]) String
typeCheckList1 gamma ns es (Failure s) = Failure s
typeCheckList1 gamma ns es (Ok (phi, t)) =
  typeCheckList2 phi t (typeCheckList gamma' ns es)
  where
    gamma' = subTypeEnv phi gamma

typeCheckList2 ::
  Subst
  -> TypeExpression
  -> Reply (Subst, [TypeExpression]) b
  -> Reply (Subst, [TypeExpression]) b
typeCheckList2 phi t (Failure s) = Failure s
typeCheckList2 phi t (Ok (psi, ts)) =
  Ok (psi `scompose` phi, (subType psi t) : ts)

typeCheckVar gamma ns x =
  Ok (idSubstitution, newInstance ns scheme)
  where
    scheme = val gamma x
    newInstance ns (Scheme scvs t) =
      let al = scvs `zip` (nameSequence ns)
          phi = alToSubst al
      in subType phi t

alToSubst ::
  AssocList [Integer] TypeVarName -> [Integer] -> TypeExpression
alToSubst al tvn
  | tvn `elem` (dom al) = TypeVar (val al tvn)
  | otherwise           = TypeVar tvn
