module Main where

import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude
import           Config
import           Domain.UseCases.GetOrdersByUserId
import           Types

main :: IO ()
main = handler

handler = getOrdersByUserIdHandler