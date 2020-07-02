{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types where

import           ClassyPrelude
import           Control.Lens
import           Data.Aeson
import           Text.Casing                    ( camel )

data OrderStatus = Pending | Paid | Processed | Delivered deriving (Show, Read, Generic, ToJSON, FromJSON)

data Order
  = Order
      { _orderId      :: Text
      , _orderUserId  :: Text
      , _orderStatus  :: OrderStatus
      , _orderAddress :: Text
      , _orderEmail   :: Email
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

data CreateOrderPayload
  = CreateOrderPayload
      { _createOrderUserId  :: Text
      , _createOrderAddress :: Text
      , _createOrderEmail   :: Text
      } deriving (Show, Generic)

instance ToJSON CreateOrderPayload where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camel . drop 12 }

instance FromJSON CreateOrderPayload where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camel . drop 12 }

makeLenses ''CreateOrderPayload


data UpdateStatusPayload
  = UpdateStatusPayload
      { _updateStatusPayloadStatus :: OrderStatus
      }
  deriving (Show, Generic)

instance ToJSON UpdateStatusPayload where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camel . drop 20 }

instance FromJSON UpdateStatusPayload where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camel . drop 20 }

makeLenses ''UpdateStatusPayload



