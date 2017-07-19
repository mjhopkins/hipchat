{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
--
-- Module: HipChat.AddOn.Types.AuthenticationMethod
--
--------------------------------------------------------------------------------

module HipChat.AddOn.Types.AuthenticationMethod where

import           Control.Lens.AsText (AsText)
import qualified Control.Lens.AsText as AsText
import           Data.Aeson          (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Ix
import           GHC.Generics

--------------------------------------------------------------------------------
-- AuthenticationMethod
--------------------------------------------------------------------------------

data AuthenticationMethod = None | JWT
  deriving (Eq, Ord, Enum, Bounded, Ix, Generic, Show)

instance AsText AuthenticationMethod where
  enc = \case
    JWT  -> "jwt"
    None -> "none"
  dec = \case
    "jwt"  -> Just JWT
    "none" -> Just None
    _      -> Nothing

instance ToJSON AuthenticationMethod where
  toJSON = AsText.toJSON

instance FromJSON AuthenticationMethod where
  parseJSON = AsText.parseJSON
