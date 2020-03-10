-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Protolude
import qualified Web.Scotty as Sc
-- import qualified Data.Text as Txt
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  let port = 80
  let settings = Warp.setPort port Warp.defaultSettings
  sapp <- scottyApp
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

scottyApp :: IO Wai.Application
scottyApp =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $
      Sc.file "index.html"

wsapp :: WS.ServerApp
wsapp pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  (msg :: Text) <- WS.receiveData conn
  WS.sendTextData conn $ ("initial> " :: Text) <> msg

  forever $ do
    WS.sendTextData conn $ "loop data"
    threadDelay $ 1 * 1000000
