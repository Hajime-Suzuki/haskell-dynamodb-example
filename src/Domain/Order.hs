module Domain.Order where

import           ClassyPrelude
import           Control.Lens
import           Data.Either.Validation
import           Text.Email.Validate            ( isValid )
import           Domain.Types
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.UUID                     as UUID

mkOrder :: CreateOrderPayload -> IO (Validation String Order)
mkOrder payload = do
  id <- UUID.toText <$> nextRandom
  return
    $   Order
    <$> pure "1234"
    <*> parseUserId (payload ^. createOrderUserId)
    <*> pure Pending
    <*> pure (payload ^. createOrderAddress)
    <*> mkEmail (payload ^. createOrderEmail)

parseUserId :: Text -> Validation String Text
parseUserId id =
  if length id > 5 then Success id else Failure "invalid user id. "

mkEmail :: Text -> Validation String Email
mkEmail email = if isValid $ encodeUtf8 email
  then Success $ Email email
  else Failure "invalid email. "

