-- | A very simple lambda calculus evaluator

module Experiment.Me.Lambda where

import Test.Hspec

data Expr
  = Application Expr Expr
  | Variable String
  | Abstraction String Expr  -- also called Lambda
  deriving (Show, Eq)

identity = Abstraction "x" (Variable "x")
applied = Application identity (Variable "y")

-- replace :: Expr -> Expr -> Expr
-- replace (Abstraction str expr') expr =
--   case expr of
--     Variable str'           -> Variable str'
--
--     Abstraction str' expr'' -> replace (Variable str) expr''
--
--     Application exprA exprB ->
--       Application (replace (Variable str) exprA) (replace (Variable str) exprB)
--
-- replace (Variable str) expr =
--   case expr of
--     Variable _ -> Variable str
--
--     Abstraction str' expr'' -> replace (Variable str) expr''
--
--     Application exprA exprB ->
--       Application (replace (Variable str) exprA) (replace (Variable str) exprB)
rename :: Expr -> Expr -> Expr
rename (Variable a) (Variable b) =
  if Variable a == Variable b then Variable a else Variable b


eval :: Expr -> Expr
eval (Application (Abstraction str expr') expr) =
  case expr' of
    Variable x -> if x == str then expr else Variable x
    Application a b ->
      -- if the abstraction expr is application, we need to dig down
      Application
        (eval (Application (Abstraction str a) expr))
        (eval (Application (Abstraction str b) expr))
    Abstraction str'' expr'' ->
      -- We want to keep the original abstraction
      Abstraction str'' (eval (Application (Abstraction str expr'') expr))
eval (Variable x) = Variable x
eval expr = error (show expr)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "eval" $ do
    it "works for app expr replacement" $ do
      eval (Application (Abstraction "x" (Variable "x")) (Variable "y"))
      `shouldBe`
      Variable "y"
    it "works for app expr replacement (identity)" $ do
      eval (Application (Abstraction "x" (Variable "x")) identity)
      `shouldBe`
      identity
    it "keeps the non-replacable variable" $ do
      eval (Application
            (Abstraction "y" (Application (Variable "x") (Variable "y")))
            (Variable "b"))
        `shouldBe`
        Application (Variable "x") (Variable "b")
    it "works with the abstraction containing an abstraction" $ do
      eval (Application
             (Abstraction "y" (Abstraction "x" (Application (Variable "x") (Variable "y"))))
             (Variable "b")
           )
        `shouldBe`
        Abstraction "x" (Application (Variable "x") (Variable "b"))

--  describe "replace" $ do
--    it "works for simple" $ do
--      replace (Abstraction "y" (Variable "y")) (Variable "x")
--      `shouldBe`
--      Variable "x"
--    it "works for " $ do
--      replace (Abstraction "y" (Application (Variable "y") (Variable "z"))) (Variable "x")
--      `shouldBe`
--      Application (Variable "x") (Variable "x")
