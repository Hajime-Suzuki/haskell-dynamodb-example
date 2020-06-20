{-# LANGUAGE FlexibleContexts #-}

module Repositories.Common where
import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Network.AWS             hiding ( send )
import           Network.AWS.DynamoDB
import           Types

dbEnv :: (Applicative m, MonadIO m, MonadCatch m) => m Env
dbEnv =
  let dynamo = setEndpoint False "localhost" 8000 dynamoDB
  in  newEnv (FromFile "dynamodb-example" ".env.aws-example")
        <&> configure dynamo
        <&> envRegion
        .~  Frankfurt


handleReq :: (Repository m, AWSRequest a) => a -> m (Rs a)
handleReq a = do
  env <- asks (^. configEnv)
  runResourceT . runAWST env $ send a
