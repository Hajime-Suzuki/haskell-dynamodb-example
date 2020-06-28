module Domain.UseCases.GetOrdersByUserId where

import           ClassyPrelude
import           Config
import           Domain.Order
import           Repositories.Order
import           Text.Pretty.Simple             ( pPrint )
import           Types
import           Data.Aeson.Encode.Pretty

getOrdersByUserIdHandler :: IO ()
getOrdersByUserIdHandler = do
  config <- getConfig
  res    <- runUseCase2 config $ getOrdersByUserIdUseCase "1234"
  pPrint $ encodePretty res

getOrdersByUserIdUseCase :: Text -> UseCase2 [Order]
getOrdersByUserIdUseCase = getByUserId

