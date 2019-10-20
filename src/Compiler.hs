module Compiler where

import Language
import Parser

intersperse :: String -> [String] -> String
intersperse _ [] = ""
intersperse c [x] = x
intersperse c (x:xs) = x <> c <> (intersperse c xs)

-- EAp (EAp (EVar "multiply") (EVar "x")) (EVar "y"))
compileExpr :: Expr Name -> String
compileExpr (EVar a) = " " <> a <> " "
compileExpr (EAp exprA exprB) =
  "return (" <>
  (compileExpr exprA) <>
  (compileExpr exprB) <>
  ")"

compile' :: ScDefn Name -> String
compile' (name, [], (EVar v)) =
  "var " <> name <> " = " <> v <> ";\n"
compile' (name, vars, expr) =
  "function " <> name
  <> "("
  <> (intersperse "," vars)
  <> ") {\n" <>
  (compileExpr expr)
  <> "}\n"

compile :: CoreProgram -> String
compile = concat . fmap compile'

testRun :: String -> String
testRun = compile . syntax . (clex 0)

test0 = syntax $ clex 0 "a = 0 ;"

test1 = testRun "a = 0 ;"

test2 = syntax $ clex 0 "square x = multiply x y ;"

test3 = testRun "square x = multiply x y ;"
