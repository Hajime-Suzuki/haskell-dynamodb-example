{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Repositories.Types where

import           ClassyPrelude
import           Types
import           Domain.Order
import           Control.Lens
import           Control.Monad.Catch
import           Domain.Types

data UpdateInput
  = UpdateInput
      { _updateOrderInputStatus  :: Maybe OrderStatus
      , _updateOrderInputAddress :: Maybe Text
      , _updateOrderInputEmail   :: Maybe Email
      }

makeLenses ''UpdateInput

defaultUpdateOrderInput = UpdateInput Nothing Nothing Nothing

type Repository' a = ReaderT Config IO a
type Repository m = (MonadReader Config m, MonadUnliftIO m, MonadCatch m)
