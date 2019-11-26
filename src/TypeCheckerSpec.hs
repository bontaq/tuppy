module TypeCheckerSpec where

import Test.Hspec

import TypeChecker
import Language
import Parser

main :: IO ()
main = hspec spec

translate (name, vars, expr) = expr
translatedCore :: CoreProgram -> [VExpr]
translatedCore = map translate

runTest :: TypeEnv -> String -> String
runTest typeEnv strProgram =
  let result = typeCheckCore $ syntax $ clex 0 strProgram
  in case result of
    t -> show t
    -- (Ok (_, t)) -> "Ok: " <> show t
    -- (Failure x) -> "Failed: " <> show x

spec :: Spec
spec = do
  describe "TypeChecker" $ do
    -- it "works for an integer" $ do
    --   runTest [] "main = 3 ;"
    --   `shouldBe`
    --   "Ok: " <> show [int]
    -- it "works for a string" $ do
    --   runTest [] "main = \"hello\" ;"
    --   `shouldBe`
    --   "Ok: " <> show [string]
    -- it "works for multiply" $ do
    --   runTest
    --     [(nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
    --     "main = multiply 3 3 ;"
    --   `shouldBe`
    --     "Ok: " <> show [int]
    -- it "fails for bad multiply" $ do
    --   runTest
    --     [(nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
    --     "main = multiply 3 \"x\" ;"
    --   `shouldBe`
    --     "Failed: \"Ap2 Could not unify: TCN: int TS: [] TCN: string TS: []\""
        -- now THAT is one ugly error message

    -- it "works for multiple lines with a const" $ do
    --   runTest
    --     [ (nameToNumber "multiply"
    --     , Scheme [] (arrow int (arrow int int))) ]
    --     "test = 1 ; main = multiply test ;"
    --   `shouldBe`
    --     "Ap2 Could not unify: TCN: int TS: [] TCN: string TS: [] : could not typecheck"

    it "works for more complicated multiple lines" $ do
      runTest
        [ (nameToNumber "multiply"
        , Scheme [] (arrow int (arrow int int))) ]
        -- "square x = multiply x x ;"
        "square x = multiply x x ; main = square 2 ;"
      `shouldBe`
        ""
    it "works" $ do
      2 `shouldBe` 2
  -- describe "transformExpr" $ do
  --   it "takes free variables and makes them applications" $ do
  --     transformExpr ("square", ["x"], EAp (EAp (EVar "multiply") (EVar "x")) (EVar "x"))
  --     `shouldBe`
  --     EVar "x"
