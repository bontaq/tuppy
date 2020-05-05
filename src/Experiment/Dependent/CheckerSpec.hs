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
  describe "checking the id function" $ do
    it "works without application" $ do
      let
        id'' = Ann id' (Inf $ Pi (free "a") (free "a")) -- :@: free "y"
      typeInfer0 [ (Global "a", VStar) ] id''
      `shouldBe`
      Right VLit
    it "works when applied" $ do
      let
        -- Ann_ (Lam_ (Lam_ (Inf_ (Bound_ 0)))) (Inf_ (Pi_ (Inf_ Star_) (Inf_ (Pi_ (Inf_ (Bound_ 0)) (Inf_ (Bound_ 1))))))
        id'' = Ann id' (Inf $ Pi (free "a") (free "a")) -- :@: free "y"
      typeInfer0 [ (Global "string", VStar)
                 , (Global "string", VStar)
                 , (Global "y", VNeutral (NFree $ Global "string"))]
        (id'' :@: free "y")
      `shouldBe`
      Right VLit
