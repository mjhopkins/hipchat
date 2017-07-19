{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.API.Capabilities
-- Description: Binding to get the capabilities descriptor for the HipChat
-- server.
--------------------------------------------------------------------------------

module  HipChat.API.Capabilities where

import           HipChat.API.Types (CapabilitiesDescriptor)

import           Data.Proxy        (Proxy (..))
import           Servant.API       (Get, JSON, (:>))
import           Servant.Client    (ClientM, client)

-- NB the capabilitiesUrl received as part of installation flow is the full URL
-- Hence no need for "v2" :> "capabilities"
type CapabiltiesAPI =
  Get '[JSON] CapabilitiesDescriptor

type CapabiltiesAPI' =
  "v2" :> "capabilities" :> Get '[JSON] CapabilitiesDescriptor

   -- | Get HipChat server capabilities descriptor given a capabilitiesUrl
getCapabilities :: ClientM CapabilitiesDescriptor
getCapabilities = client (Proxy :: Proxy CapabiltiesAPI)

-- | Get HipChat server capabilities descriptor given a base URL
getCapabilities' :: ClientM CapabilitiesDescriptor
getCapabilities' = client (Proxy :: Proxy CapabiltiesAPI')
