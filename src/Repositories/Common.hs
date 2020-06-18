module Repositories.Common where
import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Network.AWS
import           Network.AWS.DynamoDB

dbEnv :: (Applicative m, MonadIO m, MonadCatch m) => m Env
dbEnv =
  let dynamo = setEndpoint False "localhost" 8000 dynamoDB
  in  newEnv (FromFile "opc-sandbox" "/Users/hajime/.aws/credentials")
        <&> configure dynamo
        <&> envRegion
        .~  Frankfurt
