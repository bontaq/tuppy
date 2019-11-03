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
replaceEnding (x:xs) = [x] <> replaceEnding xs

run :: Args -> IO ()
run (Args compile file) = do
  f <- readFile file
  let js = handleCompile f
      newFile = replaceEnding file
  putStrLn newFile
  writeFile newFile js
  putStrLn "Ok!"
