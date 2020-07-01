{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Order where

import           ClassyPrelude
import           Control.Lens
import           Data.Aeson              hiding ( Success )
import           Data.Either.Validation
import           Text.Casing                    ( camel )
import           Text.Email.Validate            ( isValid )
import           Domain.Types


mkOrder :: Text -> Text -> Validation String Order
mkOrder uId email =
  Order
    <$> pure "id1234"
    <*> parseUserId uId
    <*> pure Pending
    <*> pure "address"
    <*> mkEmail email

parseUserId :: Text -> Validation String Text
parseUserId id =
  if length id > 5 then Success id else Failure "invalid user id. "

mkEmail :: Text -> Validation String Email
mkEmail email = if isValid $ encodeUtf8 email
  then Success $ Email email
  else Failure "invalid email. "

