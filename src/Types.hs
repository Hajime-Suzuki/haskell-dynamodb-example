{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
      { _configEnv :: Env
      , _configTableName :: Text
      , _configGSI1Name :: Text
      }

makeLenses ''Config

data CreateOrderError = InvalidUserId | InvalidAddress | InvalidEmail deriving(Show)

data AppError = AppError1 | AppError2 | ParsingRecordError deriving (Show, Generic)
instance Exception AppError

instance ToJSON AppError where
  toJSON = genericToJSON defaultOptions




newtype UseCase2 a = UseCase2 {
  unUseCase2 ::  ReaderT Config IO a -- TODO: type exception
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadUnliftIO, MonadCatch, MonadThrow)


runUseCase2 :: Config -> UseCase2 a -> IO a
runUseCase2 r = flip runReaderT r . unUseCase2

