module Experiment.Dependent.CheckerSpec where

import Test.Hspec
import Experiment.Dependent.Checker

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "checking a string" $ do
    it "works" $ do
      typeInfer0 [] (LitText "hello")
        `shouldBe`
        Right VLit
