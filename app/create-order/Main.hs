module Main where

import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude
import           Config
import           Domain.UseCases.CreateOrder
import           Types

main :: IO ()
main = handler

handler = createOrderHandler

