{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


--------------------------------------------------------------------------------
-- |
-- Module: HipChat.AddOn.WebPanel
--
-- WebPanels aka sidebars
--
-- https://www.hipchat.com/docs/apiv2/webpanels
-- https://developer.atlassian.com/hipchat/guide/sidebar
--------------------------------------------------------------------------------


module HipChat.AddOn.WebPanel where

import           HipChat.AddOn.Types (AuthenticationMethod, Icon, Name)
import           HipChat.Types       (Key, URL)
import           HipChat.Util        (ToFromJSON)

import           Control.Lens.AsText (AsText)
import qualified Control.Lens.AsText as AsText
import           Data.Aeson          (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Default        (Default)
import           Data.String         (IsString, fromString)
import           GHC.Generics        (Generic)

data WebPanelLocation = RightSidebar
  deriving (Eq, Show, Generic, Default)

instance AsText WebPanelLocation where
  enc _ = "hipchat.sidebar.right"

  dec "hipchat.sidebar.right" = Just RightSidebar
  dec _                       = Nothing

instance IsString WebPanelLocation where
  fromString = AsText.fromString

instance ToJSON WebPanelLocation where
  toJSON = AsText.toJSON

instance FromJSON WebPanelLocation where
  parseJSON = AsText.parseJSON

data WebPanel = WebPanel
  { webPanelAuthentication :: Maybe AuthenticationMethod
  -- ^ The authentication method for this webpanel. Defaults to JWT.
  , webPanelIcon           :: Maybe Icon
  -- ^ Icon to display on the left side of the webPanel title.
  , webPanelKey            :: Key
  -- ^ Unique key (in the context of the integration) to identify this webPanel. Valid length range: 1 - 40.
  , webPanelLocation       :: WebPanelLocation
  -- ^ The location of this webPanel Valid values: hipchat.sidebar.right.
  , webPanelName           :: Name
  -- ^ The display name of the webPanel.
  , webPanelUrl            :: URL
  -- ^ The URL of the resource providing the view content.
  , webPanelWeight         :: Maybe Int
  -- ^ Determines the order in which webPanel appear.
  -- Web panels are displayed top to bottom or left to right in order of ascending weight.
  -- Defaults to 100.
  } deriving (Eq, Generic, Show)

mkWebPanel :: Key -> Name -> URL -> WebPanel
mkWebPanel key' name url = WebPanel Nothing Nothing key' RightSidebar name url Nothing

instance ToFromJSON WebPanel
