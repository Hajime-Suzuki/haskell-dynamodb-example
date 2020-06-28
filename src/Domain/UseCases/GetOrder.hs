{-# LANGUAGE FlexibleContexts #-}

module Domain.UseCases.GetOrder where

import           ClassyPrelude
import qualified Repositories.Order            as OrderRepo
import           Types
import           Domain.Order

getOrderUseCase :: Text -> UseCase2 (Maybe Order)
getOrderUseCase = OrderRepo.getByOrderId
