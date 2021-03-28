-- |

module Experiment.Me.Checker where

import Experiment.Me.Language
import Experiment.Me.Parser

type Context = [(String, Expr String)]

toProgram :: String -> CoreProgram
toProgram str = syntax $ clex 0 0 str

annotate' :: Context -> CoreScDefn -> Context
annotate' ctx (name, vars, expr) = case expr of
  ENum _ -> [(name, Ann name (Free "Number") expr)]
  EStr _ -> [(name, Ann name (Free "String") expr)]
  ELam vars expr -> undefined
    -- so I think here we wanna do debruijn?
    -- we should rewrite all variables to something?

annotate :: CoreProgram -> Context
annotate = foldr (\expr ctx -> (annotate' ctx expr) <> ctx) []

runTest :: String -> Context
runTest = annotate . toProgram

test1 = runTest "x = 1"
test2 = runTest "x = \"hello\""

-- next!
