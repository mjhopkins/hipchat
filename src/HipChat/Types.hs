{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.Types
-- Description: Common types used in both the HipChat REST API and Add-on API.
--
--------------------------------------------------------------------------------

module HipChat.Types
  ( Room(..)
  , RoomId
  , URL
  , (/)
  , parseURL
  , toBaseUrl
  , Vendor(..)
  , name
  , url
  , Key(..)
  , keyQQ
  , OAuth2Provider(..)
  , authorizationUrl
  , tokenUrl
  ) where

import           HipChat.Types.Key
import           HipChat.Types.URL
import           HipChat.Util      (ToFromJSON)

import           Control.Lens.TH
import           Data.Text         (Text)
import           GHC.Generics
import           Prelude           hiding ((/))
import           Web.HttpApiData

--------------------------------------------------------------------------------
-- Room and RoomId
--------------------------------------------------------------------------------

-- | The id or url-encoded name of the room
-- Valid length range: 1 - 100.
data Room = RoomNum RoomId | RoomName Text
  deriving (Eq, Show)

instance ToHttpApiData Room where
  toUrlPiece (RoomNum i)  = toUrlPiece i
  toUrlPiece (RoomName t) = t

type RoomId = Int

--------------------------------------------------------------------------------
-- Vendor
--------------------------------------------------------------------------------

data Vendor = Vendor
  { vendorUrl  :: URL  -- ^ The vendor's home page URL
  , vendorName :: Text -- ^ The vendor display name
  } deriving (Show, Eq, Generic)

instance ToFromJSON Vendor

makeLensesWith camelCaseFields ''Vendor

--------------------------------------------------------------------------------
-- OAuth2Provider
--------------------------------------------------------------------------------

data OAuth2Provider = OAuth2Provider
  { oAuth2ProviderAuthorizationUrl :: Maybe URL
  , oAuth2ProviderTokenUrl         :: Maybe URL
  } deriving (Show, Eq, Generic)

instance ToFromJSON OAuth2Provider

makeLensesWith camelCaseFields ''OAuth2Provider
