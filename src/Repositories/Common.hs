{-# LANGUAGE FlexibleContexts #-}

module Repositories.Common where
import           ClassyPrelude
import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Network.AWS             hiding ( send )
import           Network.AWS.DynamoDB
import           Types
import           Repositories.Types
import           Control.Lens
import           System.Environment             ( lookupEnv )

type FromDB a = HashMap Text AttributeValue -> Maybe a

dbEnv :: (Applicative m, MonadIO m, MonadCatch m) => m Env
dbEnv = do
  stage <- liftIO $ lookupEnv "STAGE"

  case stage of
    Just "dev"  -> newEnv Discover <&> envRegion .~ Frankfurt
    Just "prod" -> newEnv Discover <&> envRegion .~ Frankfurt
    _ ->
      let dynamo = setEndpoint False "localhost" 8000 dynamoDB
      in  newEnv (FromFile "dynamodb-example" ".env.aws-example")
            <&> configure dynamo
            <&> envRegion
            .~  Frankfurt


handleReq :: (Repository m, AWSRequest a) => a -> m (Rs a)
handleReq a = do
  env <- asks (^. configEnv)
  runResourceT . runAWST env $ send a


attrS :: Maybe Text -> AttributeValue
attrS v = attributeValue & avS .~ v

attrSJust :: Text -> AttributeValue
attrSJust v = attributeValue & avS ?~ v
