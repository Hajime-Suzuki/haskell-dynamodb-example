module Main where

import           Lib
import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude
import           Config

main :: IO ()
main = handler

handler = do
  config <- getConfig
  runReaderT getTablesUseCaseTest config

