{-# LANGUAGE FlexibleContexts #-}

module Domain.UseCases.CreateOrder where


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

createOrderHandler :: IO ()
createOrderHandler = do
  config <- getConfig
  v      <- runUseCase2 config (createOrder2 "123456" "test@test.com")
  pPrint $ encodePretty v


createOrder2 :: Repository m => Text -> Text -> m Order
createOrder2 userId email = do
  let vOrder = mkOrder userId email
  case vOrder of
    Failure e     -> throwString e -- TODO: change to throwError when ExceptT is added to the stack
    Success order -> do
      res <- OrderRepo.save order
      return order
