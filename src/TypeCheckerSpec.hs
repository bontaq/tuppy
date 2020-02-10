{-# LANGUAGE QuasiQuotes #-}

module TypeCheckerSpec where

import Test.Hspec
import Text.RawString.QQ

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
  let result = (flip typeCheckCore) typeEnv $ syntax $ clex 0 0 strProgram

  in case result of
    -- t -> show t
    (Ok t) -> "Ok: " <> (show $ (fmap (\(name, rest) -> (numberToName name, rest))) t)
    (Failure x) -> "Failed: " <> show x

spec :: Spec
spec = do
  describe "TypeChecker" $ do
    -- it "works for an integer" $ do
    --   runTest [] "main = 3"
    --   `shouldBe`
    --   "Ok: [(\"main\",Scheme [] (TypeConstructor \"int\" []))]"
    -- it "works for a string" $ do
    --   runTest [] "main = \"hello\""
    --   `shouldBe`
    --   "Ok: [(\"main\",Scheme [] (TypeConstructor \"string\" []))]"
    -- it "works for multiply" $ do
    --   runTest
    --     [(nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
    --     "main = multiply 3 3"
    --   `shouldBe`
    --   "Ok: [(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"main\",Scheme [] (TypeConstructor \"int\" []))]"
    -- it "fails for bad multiply" $ do
    --   runTest
    --     [(nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
    --     "main = multiply 3 \"x\""
    --   `shouldBe`
    --     "Failed: \"Ap2 Could not unify: TCN: int TS: [] TCN: string TS: [] : could not typecheck\""
    --     -- now THAT is one ugly error message

    -- it "works for multiple lines with a const" $ do
    --   runTest
    --     [ (nameToNumber "multiply"
    --     , Scheme [] (arrow int (arrow int int))) ]
    --     "test = 1 \nmain = multiply test"
    --   `shouldBe`
    --     "Ok: [(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"test\",Scheme [] (TypeConstructor \"int\" [])),(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"test\",Scheme [] (TypeConstructor \"int\" [])),(\"main\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]))]"

    -- it "works for more complicated multiple lines" $ do
    --   runTest
    --     [ (nameToNumber "multiply"
    --     , Scheme [] (arrow int (arrow int int))) ]
    --     "square x = multiply x x\nmain = square 2"
    --   `shouldBe`
    --     "Ok: [(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"square\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []])),(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"square\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []])),(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

    -- it "works for generics" $ do
    --   runTest
    --     []
    --     "id x = x \ntesta = id 1\ntestb = id 2"
    --   `shouldBe`
    --     "Ok: [(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [23],TypeVar [23]])),(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [23],TypeVar [23]])),(\"testa\",Scheme [] (TypeConstructor \"int\" [])),(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [23],TypeVar [23]])),(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [23],TypeVar [23]])),(\"testa\",Scheme [] (TypeConstructor \"int\" [])),(\"testb\",Scheme [] (TypeConstructor \"int\" []))]"

    -- it "works for generics in a let" $ do
    --   runTest
    --     []
    --     "main = let id x = x in id 1"
    --   `shouldBe`
    --     "Ok: [(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

    it "works for double application" $ do
      runTest
        []
        "main = let id x = x in id id \"hello world\""
      `shouldBe`
        "Ok: [(\"main\",Scheme [] (TypeConstructor \"string\" []))]"

    it "works for this" $ do
      runTest
        []
        "main = let id x = x in id"
        `shouldBe`
        "Ok"

    it "works for double application in normal non-let form" $ do
      runTest
        []
        [r|
id x = x

main = id id "hello world"
          |]
        `shouldBe`
        "Ok"

--     it "works for application" $ do
--       runTest
--         []
--         "apply f g = f g\nid x = x\ntesting = apply id 1"
--       `shouldBe`
--        "Ok: [(\"apply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]],TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]]])),(\"apply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]],TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]]])),(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]])),(\"apply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]],TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]]])),(\"apply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]],TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]]])),(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]])),(\"testing\",Scheme [] (TypeConstructor \"int\" []))]"

--     it "works for id in let" $ do
--       runTest
--         []
--         [r|
-- main =
--   let id x = x
--   in id 1
--           |]
--       `shouldBe`
--         "Ok: [(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

--     it "works for id" $ do
--       runTest
--         []
--         [r|
-- id x = x

-- main = id 1
--           |]
--       `shouldBe`
--         "Ok: [(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]])),(\"id\",Scheme [] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]])),(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

  -- describe "transformExpr" $ do
  --   it "takes free variables and makes them applications" $ do
  --     transformExpr ("square", ["x"], EAp (EAp (EVar "multiply") (EVar "x")) (EVar "x"))
  --     `shouldBe`
  --     EAp (EAp (EAp (EVar "multiply") (EVar "x")) (EVar "x")) (EVar "x")
