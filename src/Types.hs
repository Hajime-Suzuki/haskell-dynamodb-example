{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           ClassyPrelude
import           Conduit                        ( ResourceT
                                                , runResourceT
                                                )
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Except
import           Data.Aeson

data Config
  = Config
      { _configEnv       :: Env
      , _configTableName :: Text
      , _configGSI1Name  :: Text
      }

makeLenses ''Config

data CreateOrderError = InvalidUserId | InvalidAddress | InvalidEmail deriving(Show)

data AppError = ParsingRecordError deriving (Show, Generic)
instance Exception AppError

instance ToJSON AppError where
  toJSON = genericToJSON defaultOptions


