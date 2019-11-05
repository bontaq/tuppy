module TypeCheckerSpec where

import Test.Hspec

import TypeChecker
import Language
import Parser

main :: IO ()
main = hspec spec

test1 =
  let
    translate (name, vars, expr) = expr
    translatedCore = head . map translate
    typeEnv = [(nameToNumber "square", Scheme [] (arrow int int))]
    (Ok (_, t)) = typeCheck typeEnv [0] $ translatedCore $ syntax $ clex 0 "main = square 3 ;"
  in
    t

test2 =
  let
    translate (name, vars, expr) = expr
    translatedCore = map translate
    typeEnv = [
      (nameToNumber "x", Scheme [] (TypeVar (nameToNumber "x")))
      , (nameToNumber "y", Scheme [] (TypeVar (nameToNumber "y")))
      , (nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
  in
    typeCheckList typeEnv [0] $ translatedCore $ syntax $ clex 0 "square = multiply x y ;"

test3 =
  let
    translate (name, vars, expr) = expr
    translatedCore = map translate
    typeEnv = [
      (nameToNumber "x", Scheme [] (TypeVar (nameToNumber "x")))
      , (nameToNumber "y", Scheme [] (TypeVar (nameToNumber "y")))
      , (nameToNumber "multiply", Scheme [] (arrow int (arrow int int)))]
  in
    typeCheckList typeEnv [0] $ translatedCore $ syntax $ clex 0 "test = let x = 1 in x ;"

test4 =
  let
    translate (name, vars, expr) = expr
    translatedCore = map translate
    typeEnv = []
  in
    typeCheckList typeEnv [0] $ translatedCore $ syntax $ clex 0 "test = \"sup\" ;"

runTest test =
  case test of
    (Ok (_, t)) -> "Ok! " <> show t
    (Failure x) -> "Failed! " <> show x

spec :: Spec
spec = do
  describe "TypeChecker" $ do
    it "works for square" $ do
      TypeCheckerSpec.test1 `shouldBe` int
    it "works for multiply" $ do
      runTest TypeCheckerSpec.test2
      `shouldBe`
      "Ok! [TypeConstructor \"int\" []]"
    it "works for let" $ do
      runTest TypeCheckerSpec.test3
      `shouldBe`
      "Ok! [TypeConstructor \"int\" []]"
    it "works for string" $ do
      runTest TypeCheckerSpec.test4
      `shouldBe`
      "Ok! [TypeConstructor \"string\" []]"
