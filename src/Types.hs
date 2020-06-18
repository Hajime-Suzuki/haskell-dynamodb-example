{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Aeson
import           ClassyPrelude
import           Text.Casing                    ( camel )
import           Control.Lens

data OrderStatus = Pending | Paid | Processed | Delivered deriving (Show, Generic, ToJSON)

data Order
  = Order
      { _orderId     :: Text
      , _orderUserId :: Text
      , _orderStatus :: OrderStatus
      }
  deriving (Show, Generic)

instance ToJSON Order where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camel . drop 6 }

makeLenses ''Order
