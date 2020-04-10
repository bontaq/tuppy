{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

-- websocket stuff
import qualified Web.Scotty as Sc
import qualified Data.Text as Txt
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- file watching
import System.FSNotify

-- language
import Compiler (compile)
import Parser   (syntax, clex)

import Data.Aeson


data ServerMsg = ServerMsg {
  name :: String
  , code :: String
  } deriving Generic

instance ToJSON ServerMsg where

--
-- Server
--
wsapp :: TChan ServerMsg -> WS.ServerApp
wsapp fromControl pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  (msg :: Txt.Text) <- WS.receiveData conn
  WS.sendTextData conn $ ("initial> " :: Txt.Text) <> msg

  forever $ do
    msg <- atomically $ readTChan fromControl
    WS.sendTextData conn $ (decodeUtf8 (encode msg))
    threadDelay $ 1 * 1000000

scottyApp :: IO Wai.Application
scottyApp =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $
      Sc.file "./repl/index.html"

runServer :: (TChan ServerMsg) -> IO ()
runServer fromControl = do
  let port = 8000
  let settings = Warp.setPort port Warp.defaultSettings
  sapp <- scottyApp
  Warp.runSettings settings
    $ WaiWs.websocketsOr
    WS.defaultConnectionOptions
    (wsapp fromControl)
    sapp

--
-- Files
--
handleFileEvent :: EventChannel -> IO ()
handleFileEvent event = do
  evt <- readChan event
  putStrLn . show $ evt
  handleFileEvent event

runFileWatcher :: IO ()
runFileWatcher = do
  fileEventChan <- newChan
  forkIO $ handleFileEvent fileEventChan
  withManager $ \mgr -> do
    watchDirChan
      mgr
      "./example-project"
      (const True)
      fileEventChan
      -- print
    forever $ threadDelay 1000000

--
-- Repl
--
data ReplEvent =
  Command String
  | Code String

repl :: (TChan ReplEvent) -> IO ()
repl up = forever $ do
  s <- getLine
  case s of
    ":q" -> atomically $ writeTChan up (Command ":q") -- write msg kill
    _    -> atomically $ writeTChan up (Code s)
  -- putStrLn $ "> " <> s

--
-- Connect the dots
--
handleCode :: String -> ServerMsg
handleCode raw =
  let
    syntax'        = syntax . (clex 0 0) $ raw
    compiled'      = compile syntax'
    (fnName, _, _) = head syntax'
  in
    ServerMsg {
      name=fnName
      , code=compiled'
    }

main :: IO ()
main = do
  -- setup the channels
  fromRepl <- atomically (newTChan :: STM (TChan ReplEvent))
  toRepl   <- atomically (newTChan :: STM (TChan String))

  fromFileWatcher <- atomically (newTChan :: STM (TChan String))

  fromServer <- atomically (newTChan :: STM (TChan String))
  toServer   <- atomically (newTChan :: STM (TChan ServerMsg))

  -- run the stuff
  fileWatchPid <- forkIO runFileWatcher
  -- serverPid    <- forkIO (runServer toServer)
  replPid      <- forkIO (repl fromRepl)

  forever $ do
    msg <- atomically $ readTChan fromRepl
    case msg of
      Command ":q" -> do
        mapM_ killThread [fileWatchPid, replPid]
        -- mapM_ killThread [fileWatchPid, serverPid, replPid]
        putStrLn "-- shutdown --"
      Code s -> do
        putStrLn $ "code " <> s
        atomically $ writeTChan toServer (handleCode s)
        pure ()
