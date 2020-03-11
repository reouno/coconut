{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai.Handler.Warp    ( run )
import           Network.Wai.Middleware.Cors ( cors, corsRequestHeaders, simpleCorsResourcePolicy )

import           App.BasicAuth               ( app )

main :: IO ()
main = do
  let port = 8080
  print $ "Listening at " ++ show port ++ "..."
  run port =<< cors (const $ Just policy) <$> app

policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
