module Main where

import           Lib
import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude

main :: IO ()
main = orderTest

