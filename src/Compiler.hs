module Compiler where

import Language
import Parser


-- | The compiler
--
-- It takes a CoreProgram and produces a string, that happens to be
-- valid javascript.  So far, it doesn't do anything fancy at all.

intersperse :: String -> [String] -> String
intersperse _ [] = ""
intersperse c [x] = x
intersperse c (x:xs) = x <> c <> (intersperse c xs)

compileExpr :: Expr Name -> String
-- EAp (EAp (EVar "multiply") (EVar "x")) (EVar "y"))
compileExpr (EVar a) = a <> " "
compileExpr (EStr a) = "\"" <> a <> "\""
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
