{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

--------------------------------------------------------------------------------
-- |
-- Module HipChat.AddOn.Registration
--
-- HipChat add on registration, sent as part of the add-on installation flow.
-- See: https://developer.atlassian.com/hipchat/guide/installation-flow
--------------------------------------------------------------------------------

module HipChat.AddOn.Registration where

import           HipChat.Types   (RoomId, URL)
import           HipChat.Util    (ToFromJSON)

import           Control.Lens.TH (camelCaseFields, makeLensesWith)
import           Data.Text       (Text)
import           GHC.Generics

data Registration = Registration
  { registrationOauthId         :: Text
  -- ^ OAuth client ID
  , registrationOauthSecret     :: Text
  -- ^ OAuth shared secret
  , registrationCapabilitiesUrl :: URL
  -- ^ URL for a capabilities document which lists the URL for endpoints you can
  --   use to make REST calls to this installation
  , registrationRoomId          :: Maybe RoomId
  -- ^ (optional, for room installations only): ID for the room the add-on was installed in
  , registrationGroupId         :: Int
  -- ^ ID for the HipChat group the add-on was installed in
  } deriving (Show, Eq, Generic)

instance ToFromJSON Registration

makeLensesWith camelCaseFields ''Registration
