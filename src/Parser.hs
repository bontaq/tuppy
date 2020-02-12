{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Language
import Printer
import Data.Char (isDigit, isSpace, isAlpha)
import Debug.Trace

-- read -> lex -> parse -> CoreProgram
-- parse = syntax . clex

-- utils
isIdChar  :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

isQuote :: Char -> Bool
isQuote c = c == '"'

isSingleQuote :: Char -> Bool
isSingleQuote c = c == '\''

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
-- the smallest part, which creates tokens for the parser
-- to consume.  it ignores space, comments, etc

-- integer can be ignored, could eventually be the current offset
-- for now it just means put a 0 in args
type Offset = Integer
type Indent = Integer
type Token = (Offset, Indent, String)

clex :: Offset -> Indent -> String -> [Token]

clex n o (c:cs) | isSingleQuote c =
                  [(n, o, "'"), stringToken, (n, o, "'")] <> (clex n o restCs)
  where
    stringToken = (,,) n o $ takeWhile (not . isSingleQuote) cs
    restCs = drop 1 $ dropWhile (not . isSingleQuote) cs

clex n o (c:cs) | isQuote c = [(n, o, "\""), stringToken, (n, o, "\"")] <> (clex n o restCs)
  where
    stringToken = (,,) n o $ takeWhile (not . isQuote) cs
    restCs = drop 1 $ dropWhile (not . isQuote) cs

clex n o (c:d:cs) | isEndOfLine c && (not . isSpace) d = clex 0 0 (d:cs)

clex n o (c:cs) | isSpace c = clex n 1 cs

clex n o (c:cs) | isDigit c = numToken : (clex n o restCs)
  where
    numToken = (,,) n o $ c : takeWhile isDigit cs
    restCs = dropWhile isDigit cs

clex n o (c:cs) | isAlpha c = varToken : clex n o restCs
  where
    varToken = (,,) n o $ c : takeWhile isIdChar cs
    restCs = dropWhile isIdChar cs

clex n o (c:d:cs) | isComment [c,d] = clex n o restCs
  where
    restCs = dropWhile (not . isEndOfLine) cs

clex n o (c:d:cs) | isTwoCharOp [c,d] = opToken : clex n o restCs
  where
    opToken = (,,) n o $ [c,d]
    restCs = cs

clex n o (c:cs) = (n, o, [c]) : clex n o cs

clex n o [] = []

--
-- parse (library / utilities section)
--
-- parsers return a success, meaning a token that matches
-- the parser, or nothing, meaning it failed
--
-- example of a parser that matches hello or goodbye
-- pHelloOrGoodbye :: Parser String
-- pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

type Parser a = [Token] -> [(a, [Token])]

-- parse literal
pLit :: String -> Parser String
pLit s = pSat (== s)

pNum :: Parser Int
pNum = pApply (pSat (all isDigit)) (\c -> read c :: Int)

pEmpty :: a -> Parser a
pEmpty p toks = [(p, toks)]

-- combining two parsers, returns whichever matched
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

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

-- recognize zero or more of a parser
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

-- this could be made more efficient by finishing after the first result is found
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p =
  pThen (:) p (pZeroOrMore p)

-- run a function over the values returned by a parser
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks =
  [ (f(n), toks') | (n, toks') <- p toks ]

-- second parser represents the separator, not returned
pOneOrMoreWithSep :: Parser a -> Parser String -> Parser [a]
pOneOrMoreWithSep p pSep =
  pThen combine' p $ pZeroOrMore $ (pThen combine pSep p)
  where
    combine v1 v2 = v2
    combine' e  es = e:es

-- pSatisfies
pSat :: (String -> Bool) -> Parser String
pSat f ((n, _, tok) : toks) =
  case f tok of
    True -> [(tok, toks)]
    False -> []
pSat _ _ = []

pIndent :: a -> Parser a
pIndent p (tok@(_, indent, _) : toks) =
  case indent of
    0 -> [(p, tok:toks)]
    _ -> []
pIndent p toks = [(p, toks)]

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain (e:es) = foldl EAp e es

--
-- parse (core language)
--
keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack", "=", ":"]

pVar :: Parser String
pVar = pSat (not . (flip elem) keywords)

pStr :: Parser CoreExpr
pStr = pThen3 mkStr (pLit "\"") pVar (pLit "\"")
  where
    mkStr _ x _ = EStr x

pExpr :: Parser CoreExpr
pExpr = pApply (pOneOrMore pAexpr) mkApChain

pIns :: Parser [([String], CoreExpr)]
pIns = pOneOrMore (pThen3 mkIn (pOneOrMore pVar) (pLit "=") pExpr)
  where
    mkIn v _ e = (v, e)

pLet :: Parser CoreExpr
pLet = pThen4 mkLet (pLit "let") pIns (pLit "in") pExpr
  where
    mkFn ((name:vars), expr) = (name, ELam vars expr)
    mkLet _ fns _ e = ELet False (fmap mkFn fns) e

pLambda :: Parser CoreExpr
pLambda = pThen4 mkLambda (pLit "\\") (pOneOrMore pVar) (pLit "->") pExpr
  where
    mkLambda _ vars _ e = ELam vars e

pType :: Parser CoreExpr
pType = pThen3 mkType pVar (pLit ":") (pOneOrMoreWithSep pVar (pLit "->"))
  where
    mkType name _ vs = Ann name (types vs)
    types [t]     = TFree t
    types (t:ts)  = Fun (TFree t) (types ts)

-- this is the big one, which defines each acceptable parse
-- for the language.  it goes through and tries each.
pAexpr :: Parser CoreExpr
pAexpr =
  pApply pNum ENum
  `pAlt` pStr
  `pAlt` pLambda
  `pAlt` pApply pVar EVar
  `pAlt` pType
  `pAlt` pLet

mkSc :: String   -- main (fn name)
     -> [String] -- a b  (variables)
     -> String   -- =
     -> CoreExpr -- expr
     -> (Name, [Name], CoreExpr)
mkSc name vars eq expr = (name, [], foldr ELam expr (fmap (\v -> [v]) vars))

pSc :: Parser (Name, [Name], CoreExpr)
pSc = pSc' `pAlt` pType'
  where
    pSc' = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr
    mkScFromType t@(Ann name types) = (name, [], t)
    pType' = pApply pType mkScFromType

scs :: [Token] -> [[Token]]
scs [] = []
scs (h:toks) =
  let
    sc = takeWhile (\(_, indent, _) -> indent /= 0) toks
    rest = drop (length sc) toks
  in
    (h:sc) : (scs rest)

-- type Parser a = [Token] -> [(a, [Token])]
-- type ScDefn a = (Name, [a], Expr a)
-- type Program a = [ScDefn a]
-- type CoreProgram = Program Name
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc $ pIndent []

syntax :: [Token] -> CoreProgram
syntax toks = takeFirstParse . pProgram $ toks
  where
    takeFirstParse ((prog, []) : others) = prog
    takeFirstParse (parse      : others) = takeFirstParse others
    takeFirstParse other                 = error "Syntax error"
