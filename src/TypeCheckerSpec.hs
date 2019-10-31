module TypeCheckerSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Test" $ do
    it "works" $ do
      2 `shouldBe` 2
