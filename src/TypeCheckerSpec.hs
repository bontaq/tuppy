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

--     it "works for an integer" $ do
--       runTest [] "main = 3"
--       `shouldBe`
--       "Ok: [(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

--     it "works for a string" $ do
--       runTest [] "main = \"hello\""
--       `shouldBe`
--       "Ok: [(\"main\",Scheme [] (TypeConstructor \"string\" []))]"

--     it "works for multiply" $ do
--       runTest
--         [(nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
--         "main = multiply 3 3"
--       `shouldBe`
--       "Ok: [(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

--     it "fails for bad multiply" $ do
--       runTest
--         [(nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
--         "main = multiply 3 \"x\""
--       `shouldBe`
--         "Failed: \"Ap2 Could not unify: TCN: int TS: [] TCN: string TS: [] : could not typecheck\""
--         -- now THAT is one ugly error message

--     it "works for multiple lines with a const" $ do
--       runTest
--         [ (nameToNumber "multiply"
--         , Scheme [] (arrow int (arrow int int))) ]
--         "test = 1 \nmain = multiply test"
--       `shouldBe`
--         "Ok: [(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"test\",Scheme [] (TypeConstructor \"int\" [])),(\"main\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]))]"

--     it "works for more complicated multiple lines" $ do
--       runTest
--         [ (nameToNumber "multiply"
--         , Scheme [] (arrow int (arrow int int))) ]
--         "square x = multiply x x\nmain = square 2"
--       `shouldBe`
--         "Ok: [(\"multiply\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []]])),(\"square\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"int\" []])),(\"main\",Scheme [] (TypeConstructor \"int\" []))]"


--     it "works for double application" $ do
--      runTest
--        []
--        "main = let id x = x in id id \"hello world\""
--      `shouldBe`
--        "Ok: [(\"main\",Scheme [] (TypeConstructor \"string\" []))]"


--     it "works for double application in normal non-let form" $ do
--       runTest
--         []
--         [r|
-- id x = x

-- main = id id 1
--           |]
--         `shouldBe`
--         "Ok: [(\"id\",Scheme [[0]] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]])),(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

--     it "works for const" $ do
--       runTest
--         []
--         [r|
-- const x y = y

-- main = const 2 "dog"
--           |]
--         `shouldBe`
--         "Ok: [(\"const\",Scheme [[0],[2]] (TypeConstructor \"arrow\" [TypeVar [0],TypeConstructor \"arrow\" [TypeVar [2],TypeVar [2]]])),(\"main\",Scheme [] (TypeConstructor \"string\" []))]"


--     it "works for let id" $ do
--       runTest
--         []
--         [r|
-- main =
--   let id x = x
--   in id 1
--           |]
--         `shouldBe`
--         "Ok: [(\"main\",Scheme [] (TypeConstructor \"int\" []))]"

--     it "works for application" $ do
--       runTest
--         []
--         [r|
-- apply f g x = f g x

-- id x = x

-- test = apply id id 1
--          |]
--       `shouldBe`
--        "Ok: [(\"apply\",Scheme [[0],[2],[4],[6],[8]] (TypeConstructor \"arrow\" [TypeConstructor \"arrow\" [TypeVar [4],TypeConstructor \"arrow\" [TypeVar [0],TypeVar [2]]],TypeConstructor \"arrow\" [TypeVar [4],TypeConstructor \"arrow\" [TypeVar [0],TypeVar [2]]]])),(\"id\",Scheme [[0]] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]])),(\"test\",Scheme [] (TypeConstructor \"int\" []))]"

--     it "works with an annotated type" $ do
--       runTest
--         []
--         [r|
--           main : a
--           main x = x
--           |]
--           `shouldBe`
--           "Ok: [(\"main\",Scheme [[0]] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]]))]"

  describe "ifThenElse" $ do
    -- it "fails if cond is not bool" $ do
    --   runTest
    --     [(nameToNumber "greaterThan", Scheme [] (arrow int (arrow int int)))]
    --     [r|
    --       max x y = if greaterThan x y then x else y
    --       |]
    --       `shouldBe`
    --       "Failed: \"Cond is not bool: Could not unify: TCN: int TS: [] TCN: bool TS: []lambda failedlambda failed : could not typecheck\""

--     it "works for an if then else" $ do
--       runTest
--         [(nameToNumber "greaterThan", Scheme [] (arrow int (arrow int bool)))]
--         [r|
-- id z = z
-- max x y = if greaterThan x y then id x else id y
-- test = max 1 2
--           |]
--           `shouldBe`
--           "Ok: [(\"greaterThan\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"bool\" []]])),(\"max\",Scheme [[0],[2],[4]] (TypeConstructor \"arrow\" [TypeVar [0],TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]]]))]"

--     it "works for an even check" $ do
--       runTest
--         [(nameToNumber "even", Scheme [] (arrow int bool))]
--         [r|
-- id x = x
-- max = if even 4 then id 1 else id 2
-- test = even max
--           |]
--         `shouldBe`
--         "Ok: [(\"even\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"bool\" []])),(\"id\",Scheme [[0]] (TypeConstructor \"arrow\" [TypeVar [0],TypeVar [0]])),(\"max\",Scheme [] (TypeConstructor \"int\" [])),(\"test\",Scheme [] (TypeConstructor \"bool\" []))]"

    it "works for an if then else" $ do
      runTest
        [(nameToNumber "even", Scheme [] (arrow int bool))]
        [r|
id x = x
max z n = if even 4 then id z else id n
test = max 1 2
          |]
          `shouldBe`
          "Ok: [(\"greaterThan\",Scheme [] (TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"arrow\" [TypeConstructor \"int\" [],TypeConstructor \"bool\" []]])),(\"max\",Scheme [[0],[2],[4]] (TypeConstructor \"arrow\" [TypeVar [0],TypeConstructor \"arrow\" [TypeVar [2],TypeVar [4]]]))]"
