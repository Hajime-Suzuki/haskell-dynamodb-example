module Main where

import           Lib
import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude
import           Config
import           Domain.UseCases.UpdateStatus
import           Types

main :: IO ()
main = handler

handler = updateStatusHandler

