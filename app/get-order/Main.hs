module Main where

import           Lib
import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude
import           Config
import           Domain.UseCases.GetOrder
import           Types

main :: IO ()
main = handler

handler = getOrderHandler

