module TypeChecker where

import Language
import Parser
import Debug.Trace

type TypeVarName = [Int]
type VExpr = CoreExpr
data TypeExpression = TypeVar TypeVarName
                    | TypeConstructor String [TypeExpression]
                    deriving (Show, Eq)

-- | What does it mean to have solved the types?
--
-- Imagine it like these simultaneous equations:
-- a * x1 + b * x2 = b1
-- c * x1 + d * x2 = b2
-- x1 and x2 are unknown, so the goal of of this
-- type checker with inference is to solve x1 and x2
--
-- the "unknowns" are represented by substitutions,
-- and we're saying "if we apply these subtitutions,
-- the equations will be solved"

-- represent a transformation from t1 to t2
arrow :: TypeExpression -> TypeExpression -> TypeExpression
arrow t1 t2 = TypeConstructor "arrow" [t1, t2]

int :: TypeExpression
int = TypeConstructor "int" []

string :: TypeExpression
string = TypeConstructor "string" []

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

-- | Creates a subst off a name
-- >>> idSubstitution "a"
-- TypeVar "a"
idSubstitution :: Subst
idSubstitution tvn = TypeVar tvn

-- | Substitution focused on a single variable
-- >>> delta "a" (TypeVar "a")
-- 1
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

-- | Unification
--
-- A system of type equations can be represented by a list of pairs
-- of type [(TypeExpression, TypeExpression)], with each pair
-- representing an equation of t1 == t2.
--
-- To unify the equations, we want to find the phi (substitution) of
-- subType phi t1 == subType phi t2
--
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
  | otherwise = Failure $ "Could not unify: TCN: " <> tcn <> " TS: " <> show ts <> " TCN: " <> tcn' <> " TS: " <> show ts'

unifyl :: Subst -> [(TypeExpression, TypeExpression)] -> Reply Subst String
unifyl phi eqns = foldr unify' (Ok phi) eqns
  where
    unify' eqn (Ok phi)    = unify phi eqn
    unify' eqn (Failure _) = Failure "Could not unifyl"


data TypeScheme = Scheme [TypeVarName] TypeExpression
                  deriving Show

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

val :: (Show b) => AssocList TypeVarName b -> TypeVarName -> b
-- val al k | trace ("Val " <> show al <> " Key " <> show k) False = undefined
val al k =
  let val = [ v | (k', v) <- al
                , k == k' ]
  in case length val of
    0 -> error $
      "Could not find variable: " <> (numberToName k) <> "\n"
    _ -> head val

install :: [(a, b)] -> a -> b -> [(a, b)]
install al k v = (k, v) : al

mg :: (Show b) => AssocList TypeVarName b -> [b]
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

deplete :: NameSupply -> NameSupply
deplete (n:ns) = (n+2:ns)

split :: NameSupply -> (NameSupply, NameSupply)
split ns = (0:ns, 1:ns)

nameSequence :: NameSupply -> [TypeVarName]
nameSequence ns = nextName ns : nameSequence (deplete ns)

typeCheck ::
  TypeEnv ->
  NameSupply ->
  VExpr ->
  Reply (Subst, TypeExpression) String
-- typeCheck gamma ns x | trace ("Gamma " <> show gamma <> "\nNS " <> show ns <> "\nX " <> show x <> "\n") False = undefined
typeCheck gamma ns (EVar x) = typeCheckVar gamma ns x
typeCheck gamma ns (ENum x) = typeCheckNum gamma ns x
typeCheck gamma ns (EStr x) = typeCheckStr gamma ns x
typeCheck gamma ns (EAp e1 e2) = typeCheckAp gamma ns e1 e2
-- TODO: fix cheating and only checking the first lambda variable
-- ie \x y -> * x y only x would be included and error on seeing y
typeCheck gamma ns (ELam (x:_) e) = typeCheckLambda gamma ns x e
typeCheck gamma ns (ELet isRec xs e) = typeCheckLet gamma ns xs e
typeCheck _ _ e = error $ show e

typeCheckAp gamma ns e1 e2 =
  typeCheckAp1 tvn (typeCheckList gamma ns' [e1, e2])
  where
    tvn = nextName ns
    ns' = deplete ns
typeCheckAp1 tvn (Failure s) =
  Failure "Ap1"
typeCheckAp1 tvn (Ok (phi, [t1, t2])) =
  typeCheckAp2 tvn (unify phi (t1, t2 `arrow` (TypeVar tvn)))
typeCheckAp2 tvn (Failure s) =
  Failure $ "Ap2 " <> s
typeCheckAp2 tvn (Ok phi) = Ok (phi, phi tvn)

typeCheckLambda ::
  TypeEnv ->
  NameSupply ->
  Name ->
  VExpr ->
  Reply (Subst, TypeExpression) String
typeCheckLambda gamma ns x e =
  typeCheckLambda1 tvn (typeCheck gamma' ns' e)
  where
    ns' = deplete ns
    gamma' = newBVar (x, tvn) : gamma
    tvn = nextName ns

typeCheckLambda1 tvn (Failure _) = Failure ""
typeCheckLambda1 tvn (Ok (phi, t)) =
  Ok (phi, (phi tvn) `arrow` t)

newBVar (x, tvn) = (nameToNumber x, Scheme [] (TypeVar tvn))

-- first type check all the right hand side of let ... up to in
-- then, update the environment with that information
-- and finally, type check the body with the new environment

-- difference from the 1987 paper, the xs and es are not separated
-- in our language
typeCheckLet ::
  TypeEnv
  -> NameSupply
  -> [(Name, VExpr)]
  -> VExpr
  -> Reply (Subst, TypeExpression) [Char]
typeCheckLet gamma ns xs expr =
  typeCheckLet1 gamma ns0 names expr (typeCheckList gamma ns1 es)
  where
    es = map snd xs
    names = map fst xs
    (ns0, ns1) = split ns

typeCheckLet1 gamma ns xs e (Failure _) = Failure "failed in let1"
typeCheckLet1 gamma ns xs e (Ok (phi, ts)) =
  typeCheckLet2 phi (typeCheck gamma'' ns1 e)
  where
    gamma' = subTypeEnv phi gamma
    gamma'' = addDeclarations gamma' ns0 xs ts
    (ns0, ns1) = split ns

typeCheckLet2 phi (Failure _) = Failure "failed in let2"
typeCheckLet2 phi (Ok (phi', t))
  = Ok (phi' `scompose` phi, t)

-- addDeclarations is to update the type environment, gamma,
-- so that it associates schematic types form the types ts with
-- the variables xs
-- the variables that become schematic are those that are not unknown
addDeclarations :: TypeEnv -> NameSupply -> [Name] -> [TypeExpression] -> TypeEnv
addDeclarations gamma ns xs ts =
  ((map nameToNumber xs) `zip` schemes) ++ gamma
  where
    unknowns = unknownTypeEnv gamma
    schemes = map (genBar unknowns ns) ts

genBar :: [TypeVarName] -> NameSupply -> TypeExpression -> TypeScheme
genBar unknowns ns t =
  Scheme (map snd al) t'
  where
    scvs = dedupe (typeVarsIn t) <> unknowns
    al = scvs `zip` (nameSequence ns)
    t' = subType (alToSubst al) t

dedupe (x:xs) = case x `elem` xs of
  True -> xs
  False -> x:dedupe(xs)

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

fromJust (Just a) = a
fromJust _ = error "could not find"

nameToNumber :: [Char] -> [Int]
nameToNumber =
  let numLookup = zip ['a'..'z'] [0..]
  in
    map (\x -> fromJust $ lookup x numLookup)

numberToName :: [Int] -> [Char]
numberToName =
  let charLookup = zip [0..] ['a'..'z']
  in
    map (\x -> fromJust $ lookup x charLookup)

typeCheckVar ::
  [(TypeVarName, TypeScheme)]
  -> NameSupply -> [Char] -> Reply (Subst, TypeExpression) b
typeCheckVar gamma ns x =
  Ok (idSubstitution, newInstance ns scheme)
  where
    scheme = val gamma (nameToNumber x)
    newInstance ns (Scheme scvs t) =
      let al = scvs `zip` (nameSequence ns)
          phi = alToSubst al
      in subType phi t

typeCheckNum ::
  (Show a, Show b, Eq a) =>
  [(a, TypeScheme)] -> NameSupply -> Int -> Reply (Subst, TypeExpression) b
typeCheckNum gamma ns x =
  Ok (idSubstitution, int)

typeCheckStr gamma ns x =
  Ok (idSubstitution, string)

alToSubst al tvn
  | tvn `elem` (dom al) = TypeVar (val al tvn)
  | otherwise           = TypeVar tvn

transformExpr :: CoreScDefn -> VExpr
transformExpr (_, vars, expr) = foldl (\expr v -> EAp expr (EVar v)) expr vars

compose :: Foldable t => t (b -> b) -> b -> b
compose fs v = foldl (flip (.)) id fs $ v

-- arrowize' :: [TypeExpression] -> TypeExpression

arrowize :: CoreScDefn -> Reply (Subst, TypeExpression) b -> TypeEnv
arrowize te (Ok (s, t)) | trace ("CoreSC: " <> show te <> " EX: " <> show t) False = undefined
arrowize (name, vars, _) (Ok (s, t)) = case length vars of
  0 -> [(nameToNumber name, Scheme [] t)]
  -- (arrow int (arrow int int))
  _ -> [ (nameToNumber name
        , Scheme [] (arrow firstType t)) ] -- [(nameToNumber name, Scheme [] scheme)]
    where
      first = head vars
      firstType = s (nameToNumber first)
      -- numberVars = zip vars $ map nameToNumber vars
      -- 1. show be number var
      -- 2. show be arrow
      -- scheme = arrow int (arrow int int)
      -- (a:b:c) = fmap (\(name, numName) -> TypeConstructor name $ [s numName]) numberVars
      -- t = arrow a $ arrow b int
      -- scheme' = compose [arrow, TypeVar [1], arrow]
      -- scheme = ((foldr (\v acc -> acc (s v) arrow) arrow) numberVars) $ t

typeCheckCore' :: TypeEnv -> CoreScDefn -> TypeEnv
-- typeCheckCore' te ex | trace ("TE: " <> show te <> " EX: " <> show ex) False = undefined
typeCheckCore' typeEnv coreExpr =
  let
    getName (name, _, _) = name
    getVars (_, vars, _) = vars
    getExpr (_, _, expr) = expr

    -- 1. update the type env with vars
    typeEnv' = typeEnv
      <> (map
         (\var -> (nameToNumber var, Scheme [] (TypeVar $ nameToNumber var)))
         (getVars coreExpr))

    -- 2. typecheck it
    result = typeCheck typeEnv' [0] (getExpr coreExpr)

    -- 3. arrowize the variables and function
    expr' = arrowize coreExpr result
  in
    -- 4. extend the original typeEnv with the new information or fail
    case result of
      (Ok (_, t)) -> typeEnv <> expr'
      (Failure x) -> error $ x <> " : could not typecheck"

-- typeCheckCore :: CoreProgram -> Reply (Subst, [TypeExpression]) String
typeCheckCore cp =
  let
    typeEnv =
      [ (nameToNumber "multiply", Scheme [] (arrow int (arrow int int))) ]
  in
    foldl (\a sc -> a <> (typeCheckCore' a sc)) typeEnv cp

-- helper for use with ghci
runTest' test =
  case test of
    (Ok (_, t)) -> "Ok! " <> show t
    (Failure x) -> "Failed! " <> show x
