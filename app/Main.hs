{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api (api, server)
import Db (createPool')
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Connecting to database..."
  pool <- createPool'
  putStrLn "Server running on http://localhost:8080"
  hFlush stdout
  run 8080 (serve api (server pool))
