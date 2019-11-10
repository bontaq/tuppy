module Main where

import Lib
import Parser (clex, syntax)
import TypeChecker
import Compiler

import Options.Applicative hiding (Failure)

data Args = Args
            { compile :: Bool
            , file :: String
            }

handler :: Parser Args
handler = Args
  <$> switch
  ( long "compile"
    <> short 'c'
    <> help "" )
  <*> strOption
  ( long "file"
    <> short 'f'
    <> help "The file to compile"
    <> metavar "STRING"
  )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (handler <**> helper)
      ( fullDesc
        <> progDesc "Compile a program"
        <> header "hello" )

handleCompile :: String -> String
handleCompile f =
  let syntax' = syntax . (clex 0) $ f
      tcResult = typeCheckCore syntax'
  in
    case tcResult of
      (Ok (_, _)) -> Compiler.compile syntax'
      (Failure x) -> x

replaceEnding :: [Char] -> [Char]
replaceEnding (".tp") = ".js"
replaceEnding (x:xs) = x : replaceEnding xs
replaceEnding _ = error "Filename did not end in .tp"

run :: Args -> IO ()
run (Args compile file) = do
  f <- readFile file
  let js = handleCompile f
      newFile = replaceEnding file
  putStrLn newFile
  writeFile newFile js
  putStrLn "Ok!"

--
-- various things to help with repl driven development
--

-- filename -> compiled results
-- for example:
-- replCompile "./examples/test2.tp"
replCompile :: String -> IO ()
replCompile fp = do
  f <- readFile fp
  putStrLn $ handleCompile f

-- only do the syntax step
replSyntax :: String -> IO ()
replSyntax fp = do
  f <- readFile fp
  putStrLn $ show . syntax . (clex 0) $ f

-- compile, but skip typechecking
replCompileNoTC :: String -> IO ()
replCompileNoTC fp = do
  f <- readFile fp
  putStrLn $ show . Compiler.compile . syntax . (clex 0) $ f
