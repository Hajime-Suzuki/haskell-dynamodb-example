{-# LANGUAGE FlexibleContexts #-}

module Domain.UseCases.UpdateStatus where

import           ClassyPrelude
import           Data.Either.Validation
import           Types
import           Text.Pretty.Simple             ( pPrint )
import           Domain.Order
import           Repositories.Common
import qualified Repositories.Order            as OrderRepo
import           Control.Lens
import           Network.AWS.DynamoDB.ListTables
import           Control.Monad.Trans.AWS
import           Control.Monad.Catch
import           Config
import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson.Encode.Pretty       ( encodePretty )

updateStatusHandler :: IO ()
updateStatusHandler = do
  config   <- getConfig
  mayOrder <- runUseCase2 config (updateStatusUseCase "1" Delivered)
  pPrint $ encodePretty mayOrder


updateStatusUseCase :: Text -> OrderStatus -> UseCase2 Order
updateStatusUseCase = OrderRepo.updateStatus

