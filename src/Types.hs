{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Aeson
import           ClassyPrelude
import           Text.Casing                    ( camel )
import           Conduit                        ( ResourceT
                                                , runResourceT
                                                )
import           Control.Lens
import           Network.AWS


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


data Config = Config {
  _configEnv :: Env
}

makeLenses ''Config

type Repository a = ReaderT Config IO a

type UseCase a = ReaderT Config IO a
