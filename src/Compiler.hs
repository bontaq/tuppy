module Compiler where

import Language
import Parser

intersperse :: String -> [String] -> String
intersperse _ [] = ""
intersperse c [x] = x
intersperse c (x:xs) = x <> c <> (intersperse c xs)

compileExpr :: Expr Name -> String
-- EAp (EAp (EVar "multiply") (EVar "x")) (EVar "y"))
compileExpr (EVar a) = a <> " "
compileExpr (EAp exprA exprB) =
  "("
  <> (compileExpr exprA)
  <> "("
  <> (compileExpr exprB)
  <> "))"
-- ELet False [("a",EVar "1")] (EVar "a"))
compileExpr (ELet recursive vars expr) =
  handleVars vars
  <> "\n return "
  <> (compileExpr expr)
  where
    handleVars = concat . map handleVar
    handleVar (name, expr) = "var " <> name <> " =" <> compileExpr expr <> ";"

compileExpr (ENum n) = show n

compile' :: ScDefn Name -> String
compile' (name, [], (EVar v)) =
  "var " <> name <> " = " <> v <> ";\n"
compile' (name, vars, expr) =
  "function " <> name <> "("
  <> (intersperse "," vars)
  <> ") {\n "
  <> "return " <> (compileExpr expr)
  <> "\n};\n"

compile :: CoreProgram -> String
compile = concat . fmap compile'

testRun :: String -> String
testRun = compile . syntax . (clex 0)

test0 = syntax $ clex 0 "a = 0 ;"

test1 = testRun "a = 0 ;"

test2 = syntax $ clex 0 "square x = multiply x y ;"

test3 = testRun "square x = multiply x y ;"

test4 =
  testRun "lettest x = let a = 1 in + a 1 ;"
