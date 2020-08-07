-- {-# LANGUAGE Strict #-}
module TypeChecker where

import Language
import Parser
import Debug.Trace
import Data.List

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

bool :: TypeExpression
bool = TypeConstructor "bool" []

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
subType _ te | trace ("subType: " <> show te) False = undefined
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
extend _ tvn t | trace "extend" False = undefined
extend phi tvn t
  | t == TypeVar tvn        = Ok phi
  | tvn `elem` typeVarsIn t = Failure "extend failed"
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
unify _ tc | trace ("Unify: " <> show tc) False = undefined
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
unifyl _ te | trace ("unfyl: " <> show te) False = undefined
unifyl phi eqns = foldr unify' (Ok phi) eqns
  where
    unify' eqn (Ok phi)    = unify phi eqn
    unify' eqn (Failure _) = Failure "Could not unifyl"


data TypeScheme = Scheme [TypeVarName] TypeExpression
                  deriving (Show, Eq)

unknownScheme :: TypeScheme -> [TypeVarName]
unknownScheme ts | trace ("unknownScheme: " <> show ts) False = undefined
unknownScheme (Scheme scvs t) =
  let tvars = typeVarsIn t
  in filter (\x -> not $ x `elem` scvs) tvars

subScheme :: Subst -> TypeScheme -> TypeScheme
subScheme _ ts | trace ("subScheme: " <> show ts) False = undefined
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
typeCheck gamma ns x | trace ("typeCheck Gamma " <> show gamma <> "\nNS " <> show ns <> "\nX " <> show x <> "\n") False = undefined
typeCheck gamma ns (EVar x) = typeCheckVar gamma ns x
typeCheck gamma ns (ENum x) = typeCheckNum gamma ns x
typeCheck gamma ns (EStr x) = typeCheckStr gamma ns x
typeCheck gamma ns (EAp e1 e2) = typeCheckAp gamma ns e1 e2
-- TODO: fix cheating and only checking the first lambda variable
-- ie \x y -> * x y only x would be included and error on seeing y
typeCheck gamma ns (ELam (x:_) e) = typeCheckLambda gamma ns x e
typeCheck gamma ns (ELam [] e) = typeCheckLambda gamma ns [] e
typeCheck gamma ns (ELet isRec xs e) = typeCheckLet gamma ns xs e
typeCheck gamma ns (EIf a b c) = typeCheckIfThenElse gamma ns a b c
-- TODO: Actually implementing a check
typeCheck gamma ns (Ann _ t e) = typeCheck gamma ns e
-- typeCheck _ _ e = error $ "No good: " <> show e

-- typeCheckAp typeenv ns e1 e2 | trace (show typeenv) False = undefined
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
typeCheckLambda g ns x e | trace "typeCheckLambda" False = undefined
typeCheckLambda gamma ns x e =
  typeCheckLambda1 tvn (typeCheck gamma' ns' e)
  where
    ns' = deplete ns
    gamma' = newBVar (x, tvn) : gamma
    tvn = nextName ns

typeCheckLambda1 tvn (Failure e) = Failure $ e <> "lambda failed"
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

typeCheckIfThenElse
  :: [(TypeVarName, TypeScheme)]
  -> [Int]
  -> Expr Name
  -> Expr Name
  -> Expr Name
  -> Reply (Subst, TypeExpression) [Char]
typeCheckIfThenElse gamma _ _ _ _ | trace ("typeCheckIfThenElse: " <> show gamma) False = undefined
typeCheckIfThenElse gamma ns cond leftHand rightHand =
  let (Ok (s1, condInferred)) = typeCheck gamma ns cond
      (Ok (s2, leftHandInferred)) = typeCheck gamma ns leftHand
      (Ok (s3, rightHandInferred)) = typeCheck gamma ns rightHand
      -- maybeSubst = unify idSubstitution (condInferred, bool)
      (Ok s5) = -- error "fuck"
        unify s2 (leftHandInferred, rightHandInferred)
  in
    case (Ok s5) of
      Ok subst -> Ok (s5
                      `scompose` subst
                      `scompose` s3
                      `scompose` s2
                      -- `scompose` s1
                     , s5 ns)
      Failure e -> Failure $ "Cond is not bool: " <> e

-- addDeclarations is to update the type environment, gamma,
-- so that it associates schematic types from the types ts with
-- the variables xs
-- the variables that become schematic are those that are not unknown
addDeclarations :: TypeEnv -> NameSupply -> [Name] -> [TypeExpression] -> TypeEnv
addDeclarations te ns names tes
  | trace ("addDeclare: "
           <> show te <> " "
           <> show ns <> " "
           <> show names <> " "
           <> show tes) False = undefined
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

dedupe [] = []
dedupe (x:xs) = case x `elem` xs of
  True -> xs
  False -> x : dedupe(xs)

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
  let numLookup = zip (['a'..'z'] <> ['A'..'Z']) [0..]
  in
    fmap (\x -> fromJust $ lookup x numLookup)

numberToName :: [Int] -> [Char]
numberToName =
  let charLookup = zip [0..] (['a'..'z'] <> ['A'..'Z'])
  in
    fmap (\x -> fromJust $ lookup x charLookup)

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

compose :: Foldable t => t (b -> b) -> b -> b
compose fs v = foldl (flip (.)) id fs $ v

arrowize :: CoreScDefn -> Reply (Subst, TypeExpression) b -> TypeEnv
arrowize te (Ok (s, t))
  | trace ("arrowize CoreSC: " <> show te <> " EX: " <> show t) False
  = undefined
arrowize (name, vars, _) (Ok (s, t)) = case length vars of
  _ -> addDeclarations [] [0] [name] [t]

transformExpr :: CoreScDefn -> CoreScDefn
transformExpr coreSc@(name, vars, expr) =
  case length vars of
    0 -> coreSc

typeCheckCore' :: TypeEnv -> CoreScDefn -> Reply TypeEnv String
-- typeCheckCore' te ex | trace ("TE: " <> show te <> " EX: " <> show ex) False = undefined
typeCheckCore' typeEnv coreExpr =
  let
    getName (name, _, _) = name
    getVars (_, vars, _) = vars
    getExpr (_, _, expr) = expr

    -- instead of this, we turn it into a let if there are arguments
    -- case length vars of
    coreExpr' = transformExpr coreExpr

    -- 1. update the type env with vars
    -- typeEnv' = typeEnv
    --   <> (map
    --      (\var -> (nameToNumber var, Scheme [] (TypeVar $ nameToNumber var)))
    --      (getVars coreExpr))

    -- 2. typecheck it
    result = typeCheck typeEnv [0] (getExpr coreExpr)

    -- 3. arrowize the variables and function
    expr' = arrowize coreExpr result
    -- expr' = result
  in
    -- 4. extend the original typeEnv with the new information or fail
    case result of
      (Ok (_, t)) -> Ok $ typeEnv <> expr'
      (Failure x) -> Failure $ x <> " : could not typecheck"

typeCheckCore :: Foldable t => t CoreScDefn -> TypeEnv -> Reply TypeEnv String
typeCheckCore cp typeEnv =
    foldl (\typeEnv' superCombinator ->
            case typeEnv' of
              Ok te ->
                case (typeCheckCore' te superCombinator) of
                  Ok t -> Ok $ nub $ te <> t
                  Failure f -> Failure f
              Failure f -> Failure f
          ) (Ok typeEnv) cp

runTypeCheck
  :: Foldable t => t CoreScDefn
  -> [(String, TypeScheme)]
  -> [(String, TypeScheme)]
runTypeCheck cp typeEnv = namifyFinalEnv $ typeCheckCore cp typeEnv'
  where
    typeEnv' = fmap (\(n, ty) -> (nameToNumber n, ty)) typeEnv
    namifyFinalEnv env =
      case env of
        Ok a -> fmap (\(n, ty) -> (numberToName n, ty)) a
        Failure b -> error "freakout"

-- helper for use with ghci
runTest' test =
  case test of
    (Ok (_, t)) -> "Ok! " <> show t
    (Failure x) -> "Failed! " <> show x
