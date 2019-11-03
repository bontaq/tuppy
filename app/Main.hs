module Main where

import Lib
import Options.Applicative

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

run :: Args -> IO ()
run (Args compile file) = putStrLn $ (show compile) <> " " <> (show file)
