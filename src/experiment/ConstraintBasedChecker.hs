module ConstraintBasedChecker where

import Data.Holmes

--
-- Language
--
data Expr
  = EVar String
    -- application: t(t)
  | EAp Expr Expr
    -- abstraction: \x.t
  | EAb String Expr
    -- bool: true or false
  | EBool Bool
    -- condition, branch, branch
  | EIf Expr Expr Expr
    -- type annotation
  | EType Expr Type

data Type
  = Bool
  | Fun Type Type
    deriving Eq

--
-- Constraint
--


-- let's try a simple example
-- context: 1 has type Int
-- can we get Int from the following?

-- variable handling
inferType :: [(String, Type)] -> Expr -> Maybe Type
inferType ctx (EVar x) = lookup x ctx

-- bool handling
inferType _  (EBool _) = Just Bool

-- annotation handing
inferType ctx (EType expr ty) = checkType ctx expr ty

checkType :: [(String, Type)] -> Expr -> Type -> Maybe Type
checkType ctx (EIf e1 e2 e3) ty =
  case ( checkType ctx e1 Bool
       , checkType ctx e2 ty
       , checkType ctx e3 ty ) of
    (Just Bool, Just ty2, Just ty3) -> Just ty
    _ -> Nothing

checkType ctx expr ty =
  case inferType ctx expr of
    Just ty' -> if ty == ty' then Just ty else Nothing
    Nothing  -> Nothing
