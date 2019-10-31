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
    typeEnv = [("square", Scheme [] (arrow int int))]
    (Ok (_, t)) = typeCheck typeEnv "abcdef" $ translatedCore $ syntax $ clex 0 "main = square 3 ;"
  in
    t

spec :: Spec
spec = do
  describe "TypeChecker" $ do
    it "works for square" $ do
      TypeCheckerSpec.test1 `shouldBe` int
