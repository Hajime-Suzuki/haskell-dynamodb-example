module Main where

import           Lib
import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude
import           Config
import           Domain.UseCases.CreateOrder

main :: IO ()
main = handler

handler = do
  config <- getConfig
  flip runReaderT config (createOrder "123456" "test@test.com")

