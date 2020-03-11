{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

-- websocket stuff
import qualified Web.Scotty as Sc
import qualified Data.Text as Txt
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- file watching
import System.FSNotify


--
-- Server
--
wsapp :: WS.ServerApp
wsapp pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  (msg :: Txt.Text) <- WS.receiveData conn
  WS.sendTextData conn $ ("initial> " :: Txt.Text) <> msg

  forever $ do
    WS.sendTextData conn $ ("loop data" :: Txt.Text)
    threadDelay $ 1 * 1000000

scottyApp :: IO Wai.Application
scottyApp =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $
      Sc.file "./repl/index.html"

runServer :: IO ()
runServer = do
  let port = 8000
  let settings = Warp.setPort port Warp.defaultSettings
  sapp <- scottyApp
  Warp.runSettings settings
    $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

--
-- Files
--
runFileWatcher :: IO ()
runFileWatcher =
  withManager $ \mgr -> do
    watchDir
      mgr
      "."
      (const True)
      print
    forever $ threadDelay 1000000

--
-- Repl
--
repl :: (TChan String) -> IO ()
repl up = forever $ do
  s <- getLine
  case s of
    ":q" -> atomically $ writeTChan up ":q" -- write msg kill
    _    -> pure ()
  putStrLn $ "> " <> s

--
-- Connect the dots
--
main :: IO ()
main = do
  -- setup the channels
  fromRepl <- atomically (newTChan :: STM (TChan String))
  toRepl   <- atomically (newTChan :: STM (TChan String))

  fromFileWatcher <- atomically (newTChan :: STM (TChan String))

  fromServer <- atomically (newTChan :: STM (TChan String))
  toServer   <- atomically (newTChan :: STM (TChan String))

  -- run the stuff
  fileWatchPid <- forkIO runFileWatcher
  serverPid    <- forkIO runServer
  replPid      <- forkIO (repl fromRepl)

  forever $ do
    msg <- atomically $ readTChan fromRepl
    case msg of
      ":q" -> do
        killThread fileWatchPid
        killThread serverPid
        killThread replPid
        putStrLn "-- shutdown --"
