{-# LANGUAGE QuasiQuotes #-}
module CompilerSpec where

import Test.Hspec
import Text.RawString.QQ
import Compiler
import Parser

main :: IO ()
main = hspec spec

testRun :: String -> String
testRun = compile . syntax . (clex 0)
-- here we're skipping typechecking, since it's not needed

spec :: Spec
spec = do
  describe "Compiler" $ do
    it "works for a simple statement" $ do
      testRun "a = 0 ;"
      `shouldBe`
      "function a() {\n return 0\n};\n"
    it "works for a function with arguments" $ do
      testRun "square x = multiply x y ;"
      `shouldBe`
      "function square(x) {\n return ((multiply (x ))(y ))\n};\n"
      -- the nested nature of application shows up again
    it "works for a string" $ do
      testRun "test = \"hello\" ;"
      `shouldBe`
      "function test() {\n return \"hello\"\n};\n"
    it "works for a let statement" $ do
      testRun "test x = let a = 1 in + a 1 ;"
      `shouldBe`
      "function test(x) {\n var a = 1;\n return ((+ (a ))(1))\n};\n"
