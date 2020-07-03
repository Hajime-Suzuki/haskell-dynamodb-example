{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import           ClassyPrelude
import           Conduit                        ( ResourceT
                                                , runResourceT
                                                )
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Except
import           Config
import           Types


newtype UseCase a = UseCase {
  unUseCase ::  ReaderT Config (ExceptT AppError IO) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,MonadCatch, MonadThrow, MonadError AppError)


instance MonadUnliftIO UseCase where
  askUnliftIO = return $ UnliftIO
    (\useCase -> do
      config <- getConfig
      res    <- runUseCase config useCase
      case res of
        Left  e    -> throwM e
        Right res' -> return res'
    )

runUseCase :: Config -> UseCase a -> IO (Either AppError a)
runUseCase r = runExceptT . flip runReaderT r . unUseCase




