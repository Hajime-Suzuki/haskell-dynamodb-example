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
import           Data.Aeson.Embedded
import           Network.AWS
import           Text.Casing                    ( camel )
import           AWSLambda.Events.APIGateway

data Config
  = Config
      { _configEnv :: Env
      , _configTableName :: Text
      , _configGSI1Name :: Text
      }

makeLenses ''Config

newtype UseCase a = UseCase {
  unUseCase ::  ReaderT Config (ExceptT String IO) a -- TODO: type exception
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,MonadCatch, MonadThrow, MonadError String) -- TODO: add MonadUnliftIO

runUseCase :: Config -> UseCase a -> IO (Either String a)
runUseCase r = runExceptT . flip runReaderT r . unUseCase



newtype UseCase2 a = UseCase2 {
  unUseCase2 ::  ReaderT Config IO a -- TODO: type exception
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadUnliftIO, MonadCatch, MonadThrow)


runUseCase2 :: Config -> UseCase2 a -> IO a
runUseCase2 r = flip runReaderT r . unUseCase2

type Adapter
  =  APIGatewayProxyRequest (Embedded Value)
  -> IO (APIGatewayProxyResponse (Embedded Value))
