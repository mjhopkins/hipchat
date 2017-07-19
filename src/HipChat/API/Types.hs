{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.API.Types
--
-- Common types for the HipChat API
--------------------------------------------------------------------------------

module HipChat.API.Types where

import           HipChat.Types   (Key, OAuth2Provider, URL, Vendor)
import           HipChat.Util    (ToFromJSON)

import           Control.Lens.TH (camelCaseFields, makeLensesWith)
import           Data.Aeson      (Object)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

--------------------------------------------------------------------------------
-- Links
--------------------------------------------------------------------------------

data Links = Links
  { linksSelf     :: URL
  -- ^ The URL to this descriptor.
  , linksApi      :: URL
  -- ^ The URL to the REST API root.
  , linksHomepage :: Maybe URL
  -- ^The URL to human-viewable home page of this integration
  } deriving (Eq, Show, Generic)

instance ToFromJSON Links

makeLensesWith camelCaseFields ''Links

--------------------------------------------------------------------------------
-- HipchatApiProvider
--------------------------------------------------------------------------------

type AvailableScopes = Object
-- TODO

data HipchatApiProvider = HipchatApiProvider
   { hipchatApiProviderUrl             :: URL
   -- ^ The base URL for the API.
   , hipchatApiProviderAvailableScopes :: AvailableScopes
   -- ^ Scopes that this API supports.
   } deriving (Eq, Show, Generic)

instance ToFromJSON HipchatApiProvider

makeLensesWith camelCaseFields ''HipchatApiProvider

--------------------------------------------------------------------------------
-- Capabilities
--------------------------------------------------------------------------------

data Capabilities = Capabilities
 { capabilitiesOauth2Provider     :: Maybe OAuth2Provider
 -- ^ The capability of providing and accepting OAuth 2 tokens for authentication.
 , capabilitiesHipchatApiProvider :: Maybe HipchatApiProvider
 -- ^ The ability to provide the HipChat API.
 } deriving (Eq, Show, Generic)

instance ToFromJSON Capabilities

makeLensesWith camelCaseFields ''Capabilities

data CapabilitiesDescriptor = CapabilitiesDescriptor
  {
    capabilitiesDescriptorVendor       :: Maybe Vendor
    -- ^ The vendor that maintains this application
  , capabilitiesDescriptorName         :: Text
    -- ^ The display name of this application
  , capabilitiesDescriptorCapabilities :: Maybe Capabilities
    -- ^ The set of capabilities this application supports.
  , capabilitiesDescriptorLinks        :: Links
    -- ^ URLs to retrieve this and related integration information
  , capabilitiesDescriptorKey          :: Key
    -- ^ The key that uniquely identifies the application
  , capabilitiesDescriptorDescription  :: Text
    -- ^ A short description of this application
  } deriving (Eq, Show, Generic)

instance ToFromJSON CapabilitiesDescriptor

makeLensesWith camelCaseFields ''CapabilitiesDescriptor
