{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.AddOn.Capabilities
--
-- HipChat add-on capabilities descriptor, and supporting types.
--------------------------------------------------------------------------------

module HipChat.AddOn.Capabilities where

import           HipChat.AddOn.Dialog   (Dialog)
import           HipChat.AddOn.Glance   (Glance)
import           HipChat.AddOn.Types    (AuthenticationMethod (..), Name,
                                         RoomEvent)
import           HipChat.AddOn.WebPanel (WebPanel)
import           HipChat.Auth           (APIScope (..))
import           HipChat.Types          (Key, OAuth2Provider, URL, Vendor)
import           HipChat.Util           (ToFromJSON)

import           Control.Lens           hiding ((.=))

import           Data.Aeson             (FromJSON, ToJSON, Value (String),
                                         object, parseJSON, toJSON, withObject,
                                         withText, (.!=), (.:?), (.=))
import           Data.Aeson.Types       (Parser)
import           Data.Default
import           Data.Maybe             (catMaybes)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics

import qualified Data.Attoparsec.Text   as A

--------------------------------------------------------------------------------
-- ApiVersion
--------------------------------------------------------------------------------

data ApiVersion = ApiVersion Int Int
  deriving (Eq, Show, Ord)

instance Default ApiVersion where
  def = ApiVersion 1 1

instance ToJSON ApiVersion where
  toJSON (ApiVersion i j) = String . T.pack $ show i ++ ":" ++ show j

instance FromJSON ApiVersion where
  parseJSON = withText "text" parseApiVersion

parseApiVersion :: Text -> Parser ApiVersion
parseApiVersion t = case A.parseOnly apiVersionParser t of
  Left _  -> fail $ "Unable to parse as ApiVersion: " ++ T.unpack t
  Right v -> return v

apiVersionParser :: A.Parser ApiVersion
apiVersionParser =
  ApiVersion <$> A.decimal <* A.char ':' <*> A.decimal <* A.endOfInput

--------------------------------------------------------------------------------
-- Links
--------------------------------------------------------------------------------

data Links = Links
  { linksSelf     :: URL
  , linksHomepage :: Maybe URL
  } deriving (Show, Eq, Generic)

mkLinks
  :: URL  -- ^ self
  -> Links
mkLinks s = Links s Nothing

instance ToFromJSON Links

makeLensesWith camelCaseFields ''Links

--------------------------------------------------------------------------------
-- Installable
--------------------------------------------------------------------------------

data Installable = Installable
  { installableCallbackUrl       :: Maybe URL
  , installableInstalledUrl      :: Maybe URL
  , installableUninstalledUrl    :: Maybe URL
  , installableUpdateCallbackUrl :: Maybe URL
  , installableUpdatedUrl        :: Maybe URL
  , installableAllowRoom         :: Bool
  , installableAllowGlobal       :: Bool
  } deriving (Show, Eq)

makeLensesWith camelCaseFields ''Installable

instance ToJSON Installable where
  toJSON inst = object $ catMaybes
    [ ("callbackUrl"       .=) <$> (inst ^. callbackUrl)
    , ("installedUrl"      .=) <$> (inst ^. installedUrl)
    , ("uninstalledUrl"    .=) <$> (inst ^. uninstalledUrl)
    , ("updateCallbackUrl" .=) <$> (inst ^. updateCallbackUrl)
    , ("updatedUrl"        .=) <$> (inst ^. updatedUrl)
    ] <>
    [ "allowRoom"   .= (inst ^. allowRoom)
    , "allowGlobal" .= (inst ^. allowGlobal)
    ]

instance FromJSON Installable where
  parseJSON = withObject "object" $ \o -> Installable
    <$> o .:? "callbackUrl"
    <*> o .:? "installedUrl"
    <*> o .:? "uninstalledUrl"
    <*> o .:? "updateCallbackUrl"
    <*> o .:? "updatedUrl"
    <*> o .:? "allowRoom" .!= True
    <*> o .:? "allowGlobal" .!= True

mkInstallable :: Installable
mkInstallable = Installable Nothing Nothing Nothing Nothing Nothing True True

--------------------------------------------------------------------------------
-- APIConsumer
--------------------------------------------------------------------------------

data APIConsumer = APIConsumer
  { apiConsumerScopes   :: [APIScope]
  , apiConsumerFromName :: Maybe Text
  , apiConsumerAvatar   :: Maybe URL
  } deriving (Show, Eq, Generic)

instance ToFromJSON APIConsumer

makeLensesWith camelCaseFields ''APIConsumer

--------------------------------------------------------------------------------
-- Webhook
--------------------------------------------------------------------------------

-- | The ability to receive HTTP POST requests on certain events
data Webhook = Webhook
  { webhookUrl            :: URL
  -- ^ The URL to send the webhook POST to.
  , webhookEvent          :: RoomEvent
  -- ^ The event to listen for.
  , webhookPattern        :: Maybe Text
  -- ^ The regular expression pattern to match against messages. Only applicable for message events.
  , webhookKey            :: Maybe Key
  -- ^ Unique key (in the context of the integration) to identify this webhook.
  , webhookName           :: Maybe Text
  -- ^ The label for this webhook.
  , webhookAuthentication :: Maybe AuthenticationMethod
  -- ^ The authentication method for this webhook.
  -- Defaults to 'none'.
  } deriving (Show, Eq, Generic)

instance ToFromJSON Webhook

mkWebhook :: URL -> RoomEvent -> Webhook
mkWebhook url e = Webhook url e Nothing Nothing Nothing Nothing

makeLensesWith camelCaseFields ''Webhook

--------------------------------------------------------------------------------
-- Configurable
--------------------------------------------------------------------------------

-- | The ability to be user configurable via a configure dialog
data Configurable = Configurable
  { configurableUrl                     :: URL               -- ^ The URL to embed into a configure dialog via an iframe
  , configurableAllowAccessToRoomAdmins :: Maybe Bool        -- ^ If the add-on is installed globally, this property can allow the configuration page to be accessed in the room administration pages making it possible to have room specific configuration for the global add-on. This is only valid for globally installed add-ons.
  , configurableAuthentication          :: Maybe AuthenticationMethod -- ^ The authentication method for this configure page. Default: jwt
  } deriving (Show, Eq, Generic)

instance ToFromJSON Configurable

makeLensesWith camelCaseFields ''Configurable

--------------------------------------------------------------------------------
-- Capabilities
--------------------------------------------------------------------------------

newtype AdminPage = AdminPage { adminPageUrl :: URL }
  deriving (Show, Eq, Generic)

instance ToFromJSON AdminPage

-- --TODO fill in missing
data Capabilities = Capabilities
  { -- capabilitiesAction          :: [Action]
    capabilitiesAdminPage          :: Maybe AdminPage
  -- ^ The ability to be user configurable via an admin page in the menu
  , capabilitiesConfigurable       :: Maybe Configurable
  , capabilitiesDialog             :: [Dialog]
  --,  capabilitiesExternalPage :: [ExternalPage]
  -- -- ^ The ability to specify an external web page that can be targeted by an action
  --     data ExternalPage ...
  , capabilitiesGlance             :: [Glance]
  , capabilitiesHipchatApiConsumer :: Maybe APIConsumer
  -- ^ The ability to consume the HipChat API
  , capabilitiesInstallable        :: Maybe Installable
  -- ^ The capability of receiving a synchronous installation callback during integration installation.
  , capabilitiesOauth2Provider     :: Maybe OAuth2Provider
  -- , capabilitiesOauth2Consumer     :: Maybe OAuth2Consumer
  , capabilitiesWebPanel           :: [WebPanel]
  , capabilitiesWebhook            :: [Webhook]
  } deriving (Show, Eq, Generic, Default)

makeLensesWith camelCaseFields ''Capabilities

instance ToFromJSON Capabilities

--------------------------------------------------------------------------------
-- AddOn
--------------------------------------------------------------------------------

-- More properly, "add-on capability descriptor"
-- https://www.hipchat.com/docs/apiv2/capabilities
data AddOn = AddOn
  { addOnKey          :: Key                -- ^ The marketplace integration key that uniquely identifies the application, if registered
  , addOnName         :: Name               -- ^ The display name of this application
  , addOnDescription  :: Text               -- ^ A short description of this application
  , addOnLinks        :: Links              -- ^ URLs to retrieve this and related integration information
  , addOnCapabilities :: Maybe Capabilities -- ^ The set of capabilities this application supports.
  , addOnVendor       :: Maybe Vendor       -- ^ The vendor that maintains this application
  , addOnApiVersion   :: Maybe ApiVersion   -- ^ The HipChat Connect API version used by this integration. Defaults to 1:1.
  } deriving (Show, Eq, Generic)

mkAddOn
  :: Key  -- ^ key
  -> Name -- ^ name
  -> Text -- ^ description
  -> Links
  -> AddOn
mkAddOn k n d ls = AddOn k n d ls Nothing Nothing Nothing

instance ToFromJSON AddOn

makeLensesWith camelCaseFields ''AddOn
