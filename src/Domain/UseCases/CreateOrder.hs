module Domain.UseCases.CreateOrder where


import           ClassyPrelude
import           Data.Either.Validation
import           Types
import           Text.Pretty.Simple             ( pPrint )
import           Domain.Order

createOrder :: Text -> Text -> UseCase ()
createOrder userId email = do
  let order = mkOrder userId email
  pPrint order
