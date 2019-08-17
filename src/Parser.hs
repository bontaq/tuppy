{-# LANGUAGE OverloadedStrings #-}

module Parser where

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
-- parse
--
