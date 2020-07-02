
{-# LANGUAGE ConstrainedClassMethods  #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SandBox where

import           ClassyPrelude
import           Text.Pretty.Simple             ( pPrint )
import           Network.AWS                    ( runAWS
                                                , AWS
                                                , MonadAWS
                                                )
import           Network.AWS.DynamoDB.ListTables
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Except
import           Types
import           Control.Lens
import           Config
import           Control.Monad.Catch
import           Control.Monad.Except
import           Repositories.Common
import           Domain.Order
import           Data.Aeson                     ( encode )
import           Data.Either.Validation
import           Repositories.Types
import           Repositories.Order



data ERR = Error1 | Error2 deriving (Show)
instance Exception ERR

newtype UC a = UC {
  unUC ::  ReaderT Config (ExceptT ERR IO) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,MonadCatch, MonadThrow, MonadError ERR)

instance MonadUnliftIO UC where
    askUnliftIO = return $ UnliftIO
        (\uc -> do
            config <- getConfig
            v      <- runUC config uc
            case v of
                Left  e -> throwM e
                Right a -> return a
        )


runUC :: Config -> UC a -> IO (Either ERR a)
runUC r = runExceptT . flip runReaderT r . unUC

testUseCase :: IO ()
testUseCase = do
    config <- getConfig
    v      <- runUC config $ do
        pPrint "use case"
        a <- getByOrderId "1"
        pPrint a
        throwError Error1
        return a
    pPrint v
