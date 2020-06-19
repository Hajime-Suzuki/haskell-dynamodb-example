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


data Config = Config {
  _configEnv :: Env
}

makeLenses ''Config

type Repository a = ReaderT Config IO a

type UseCase a = ReaderT Config IO a
