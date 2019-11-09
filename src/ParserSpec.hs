{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Test.Hspec
import Text.RawString.QQ
import Language
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Clex" $ do

    it "produces tokens for a string" $ do
      clex 0 "hello"
      `shouldBe`
      [(0, "hello")]

    it "produces tokens ignoring spaces" $ do
      clex 0 "hello world"
      `shouldBe`
      [(0, "hello"), (0, "world")]

    it "ignores comments" $ do
      clex 0 "|| bonk"
      `shouldBe`
      []

    it "works for a larger example" $ do
      clex 0 [r|
               a x = 1
               || test comment
               lambda = \x -> y
               |]
      `shouldBe`
      [ (0,"a"), (0,"x"), (0,"="), (0,"1")
        -- that double "\\" is weird, did it escape the character?
      , (0,"lambda"), (0,"="), (0,"\\"), (0,"x"), (0,"->"), (0,"y")]

  describe "Parser" $ do
    -- this will all be replaced by a real parser library at some point
    -- but it's fun to put together from the basics
    describe "Basic examples" $ do

      it "pLit parses a literal" $ do
        -- you can see here, a parser can take something to match,
        -- and tokens -- if it works, it returns the match, otherwise
        -- no match found
        pLit "hey" [(0, "hey")] `shouldBe` [("hey", [])]
        pLit "nomatch" [(0, "hey")] `shouldBe` []

      it "pEmpty always matches" $ do
        -- here pEmpty demonstrates a nice quality, it matches
        -- and returns the original tokens for other parsers to use
        pEmpty "hey" [(0, "hey")] `shouldBe` [("hey", [(0, "hey")])]
        pEmpty "nomatch" [(0, "hey")] `shouldBe` [("nomatch", [(0, "hey")])]
        pEmpty [] [(0, "hey")] `shouldBe` [("", [(0, "hey")])]
      it "pNum turns string numbers into an integer on match" $ do
        pNum [(0, "123")] `shouldBe` [(123, [])]

      describe "pThen combines parsers with a function" $ do
        -- extremely handy for success -> doing something with that success
        -- for example, adding 2 numbers together
        it "combines numbers" $ do
          pThen (+) pNum pNum [(0, "1"), (0, "2")]
          `shouldBe`
          [(3, [])]

        -- but in our case, it's used for building up the token into
        -- our language

        it "can create our language" $ do
          let
            mkVar :: String -> String -> Expr String
            mkVar v _ = EVar v  -- we ignore the "=" match
          pThen mkVar pVar (pLit "=") [(0, "main"), (0, "=")]
          `shouldBe`
          [(EVar "main", [])]

  describe "Syntax" $ do
    -- syntax is the main function
    -- it takes a bunch of tokens, and turns them into Expr
    -- which is how we represent the language
    it "can parse a simple expression" $ do
      let toks = clex 0 "a = a ;"
      syntax toks
      `shouldBe`
      -- this says "a", with no variables, has the definition EVar "a"
      [("a", [], EVar "a")]

    it "can parse variables for a supercombinator" $ do
      -- "supercombinator" is for whatever reason how top level
      -- definitions are defined
      let toks = clex 0 "main x = x ;"
      syntax toks
      `shouldBe`
      [("main", ["x"], EVar "x")]

    it "can parse multiple supercombinators" $ do
      let toks = clex 0 [r|
                          square x = * x x ;
                          main y = square y ;
                          |]
      syntax toks
      `shouldBe`
      [ ("square", ["x"], EAp (EAp (EVar "*") (EVar "x")) (EVar "x"))
      , ("main", ["y"], EAp (EVar "square") (EVar "y")) ]
      -- here you can see an example of application and its nested
      -- nature

    it "can parse a let expression" $ do
      let toks = clex 0 "main = let x = 0 in x ;"
      syntax toks
      `shouldBe`
      [("main", [], ELet False [("x", ENum 0)] (EVar "x"))]
      -- False here means non-recursive, which is for future
      -- implementation

    it "can parse a lambda expresion" $ do
      let toks = clex 0 "main = \\x -> x ;"
      syntax toks
      `shouldBe`
      [("main", [], ELam ["x"] (EVar "x"))]

    it "can parse a number" $ do
      let toks = clex 0 "main = 0 ;"
      syntax toks `shouldBe` [("main", [], ENum 0)]

    it "can parse a string" $ do
      let toks = clex 0 "main = \"hello\" ;"
      syntax toks `shouldBe` [("main", [], EStr "hello")]
