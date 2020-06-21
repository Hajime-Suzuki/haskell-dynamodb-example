{-# LANGUAGE FlexibleContexts #-}

module Domain.UseCases.GetOrder where

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

getOrderHandler :: IO ()
getOrderHandler = do
  config   <- getConfig
  mayOrder <- runUseCase2 config (getOrderUseCase "id1234")
  pPrint $ encodePretty mayOrder


getOrderUseCase :: Text -> UseCase2 (Maybe Order)
getOrderUseCase orderId = OrderRepo.findByOrderId orderId


