module Domain.UseCases.GetOrdersByUserId where

import           ClassyPrelude
import           Config
import           Domain.Order
import           Repositories.Order
import           Text.Pretty.Simple             ( pPrint )
import           Types
import           Data.Aeson.Encode.Pretty

getOrdersByUserIdUseCase :: Text -> UseCase2 [Order]
getOrdersByUserIdUseCase = getByUserId

