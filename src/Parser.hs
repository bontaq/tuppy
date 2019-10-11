{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Language
import Printer
import Data.Char (isDigit, isSpace, isAlpha)

-- read -> lex -> parse -> CoreProgram
-- parse = syntax . clex

-- utils
isIdChar  :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` [' ', '\n', '\t']

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n'

isComment :: String -> Bool
isComment c = c == "||"

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

isTwoCharOp :: String -> Bool
isTwoCharOp s = s `elem` twoCharOps


--
-- lex
--

type Token = (Integer, String)

clex :: Integer -> String -> [Token]

clex n (c:cs) | isSpace c = clex n cs

clex n (c:cs) | isDigit c = numToken : (clex n restCs)
  where
    numToken = (,) n $ c : takeWhile isDigit cs
    restCs = dropWhile isDigit cs

clex n (c:cs) | isAlpha c = varToken : clex n restCs
  where
    varToken = (,) n $ c : takeWhile isIdChar cs
    restCs = dropWhile isIdChar cs

clex n (c:d:cs) | isComment [c,d] = comToken : clex n restCs
  where
    comToken = (,) n $ takeWhile (not . isEndOfLine) cs
    restCs = dropWhile (not . isEndOfLine) cs

clex n (c:d:cs) | isTwoCharOp [c,d] = opToken : clex n restCs
  where
    opToken = (,) n $ [c,d]
    restCs = cs

clex n (c:cs) = (n, [c]) : clex n cs

clex n [] = []


--
-- parse (library)
--

type Parser a = [Token] -> [(a, [Token])]

keywords :: [String]
keywords = ["let", "letrect", "case", "in", "of", "Pack"]

-- parse literal
pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat (not . (flip elem) keywords)

pNum :: Parser Int
pNum = pApply (pSat (all isDigit)) (\c -> read c :: Int)

-- combining two parsers, returns whichever matched
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

-- example that matches hello or goodbye
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

-- apply first parser, then second on remainder of input,
-- then combine result with the combining fn (a -> b -> c)
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
  [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                           , (v2, toks2) <- p2 toks1 ]

-- more capabilities for pthen
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks
                              , (v2, toks2) <- p2 toks1
                              , (v3, toks3) <- p3 toks2 ]

pThen4 :: (a -> b -> c -> d -> e)
       -> Parser a
       -> Parser b
       -> Parser c
       -> Parser d
       -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
  [ (combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks
                                 , (v2, toks2) <- p2 toks1
                                 , (v3, toks3) <- p3 toks2
                                 , (v4, toks4) <- p4 toks3 ]

-- example, matches on hello or goodbye + a string (like james)
pGreeting :: Parser (String, String)
pGreeting = pThen mkPair pHelloOrGoodbye pVar
  where
    mkPair hg name = (hg, name)

-- example, throws out the "!" with pThen3
pGreeting' :: Parser (String, String)
pGreeting' = pThen3 mkGreeting pHelloOrGoodbye pVar (pLit "!")
  where
    mkGreeting hg name exclamation = (hg, name)

-- recognize zero or more of a parser
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pEmpty :: a -> Parser a
pEmpty p toks = [(p, toks)]

-- this could be made more efficient by finishing after the first result is found
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p =
  pThen (:) p (pZeroOrMore p)

-- run a function over the values returned by a parser
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks =
  [ (f(n), toks') | (n, toks') <- p toks ]

-- example of use
pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

-- second parser represents the separator, not returned
-- TODO: this might need to be fixed to understand "a,b,c" and include the c
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p pSep =
  pOneOrMore $ (pThen combine p pSep)
  where
    combine v1 v2 = v1

-- pSatisfies
pSat :: (String -> Bool) -> Parser String
pSat f ((n, tok) : toks) =
  case f tok of
    True -> [(tok, toks)]
    False -> []
pSat _ _ = []

pAexpr :: Parser CoreExpr
pAexpr = pApply pVar EVar
  `pAlt` pApply pNum ENum
  `pAlt` pLet

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain (e:es) = foldl EAp e es

pExpr :: Parser CoreExpr
pExpr = pApply (pOneOrMore pAexpr) mkApChain

pIns :: Parser [(String, CoreExpr)]
pIns = pOneOrMore (pThen3 mkIn pVar (pLit "=") pExpr)
  where
    mkIn v _ e = (v, e)

pLet :: Parser CoreExpr
pLet = pThen4 mkLet (pLit "let") pIns (pLit "in") pExpr
  where
    mkLet _ vs _ e = ELet False vs e

--
-- parse (core language)
--

mkSc :: String   -- main (fn name)
     -> [String] -- a b  (variables)
     -> String   -- =
     -> CoreExpr -- expr
     -> (Name, [Name], CoreExpr)
mkSc name vars eq expr = (name, vars, expr)

-- pSc :: Parser CoreScDefn
pSc :: Parser (Name, [Name], CoreExpr)
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []) : others) = prog
    takeFirstParse (parse      : others) = takeFirstParse others
    takeFirstParse other                 = error "Syntax error"

test1 :: CoreProgram
test1 =
  syntax $ clex 0 "a = a ;"

test2 =
  syntax $ clex 0 "square x = multiply x y ;"

-- it's reversing the square (square 3) ?
test3 =
  pprint $ syntax $ clex 0 "main = square (square 3) ;"

test4 =
  syntax $ clex 0 "main = square (square 3) ;"

-- test3 :: CoreProgram
test5 =
  syntax $ clex 0 "square x = multiply x y ; main = square 3 2 ; "

test5txt = "square x = multiply x y ; main = square 3 2"
