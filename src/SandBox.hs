
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

