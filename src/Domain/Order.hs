{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Order where

import           ClassyPrelude
import           Data.Aeson              hiding ( Success )
import           Text.Casing                    ( camel )
import           Control.Lens
import           Data.Either.Validation
import           Text.Email.Validate            ( isValid )

data OrderStatus = Pending | Paid | Processed | Delivered deriving (Show, Read, Generic, ToJSON)

data Order
  = Order
      { _orderId     :: Text
      , _orderUserId :: Text
      , _orderStatus :: OrderStatus
      , _orderAddress :: Text
      , _orderEmail :: Email
      }
  deriving (Show, Generic)

instance ToJSON Order where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camel . drop 6 }

newtype Email = Email {
  _rawEmail::Text
} deriving (Show, Generic)
makeLenses ''Email

instance ToJSON Email where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camel . drop 3
                                        , unwrapUnaryRecords = True
                                        }

makeLenses ''Order


data CreateOrderError = InvalidUserId | InvalidAddress | InvalidEmail deriving(Show)

mkOrder :: Text -> Text -> Validation String Order
mkOrder uId email =
  Order
    <$> pure "id1234"
    <*> parseUserId uId
    <*> pure Pending
    <*> pure "address"
    <*> mkEmail email

parseUserId :: Text -> Validation String Text
parseUserId id =
  if length id > 5 then Success id else Failure "invalid user id. "

mkEmail :: Text -> Validation String Email
mkEmail email = if isValid $ encodeUtf8 email
  then Success $ Email email
  else Failure "invalid email. "

