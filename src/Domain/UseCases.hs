module Domain.UseCases where

import           ClassyPrelude
import           Data.Either.Validation
import           Types
import           Text.Pretty.Simple             ( pPrint )
import           Domain.Order
import qualified Repositories.Order            as OrderRepo
import           Control.Lens
import           Network.AWS.DynamoDB.ListTables
import           Control.Monad.Trans.AWS
import           Control.Monad.Catch
import           Config
import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Domain.Types
import           App


getOrderUseCase :: Text -> UseCase (Maybe Order)
getOrderUseCase = OrderRepo.getByOrderId

getOrdersByUserIdUseCase :: Text -> UseCase [Order]
getOrdersByUserIdUseCase = OrderRepo.getByUserId

createOrderUseCase :: CreateOrderPayload -> UseCase Order
createOrderUseCase payload = do
  newOrder <- liftIO $ mkOrder payload
  case newOrder of
    Failure e     -> throwString e
    Success order -> do
      res <- OrderRepo.save order
      return order

updateStatusUseCase :: Text -> UpdateStatusPayload -> UseCase Order
updateStatusUseCase = OrderRepo.updateStatus

