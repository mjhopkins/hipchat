{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

--------------------------------------------------------------------------------
-- |
-- Module HipChat.AddOn.Webhooks
--
-- Definitions for handling webhook event notifications (call backs)
--------------------------------------------------------------------------------

module HipChat.AddOn.Webhooks where

import           HipChat.AddOn.Types (RoomEvent (..))
import           HipChat.Types       (URL)
import           HipChat.Util        (ToFromJSON)

import           Control.Lens.AsText (AsText, dec, enc)
import qualified Control.Lens.AsText as AsText
import           Control.Lens.TH     (camelCaseFields, makeFields,
                                      makeLensesWith)
import           Data.Aeson          (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Default        (Default, def)
import           Data.Ix             (Ix)
import           Data.String         (IsString, fromString)
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           GHC.Generics        (Generic)

type Date = UTCTime

--e.g. f3a628d0-be81-461a-b025-41604d982310
newtype UUID = UUID Text
  deriving (Show, Eq, Generic)

instance ToFromJSON UUID

--------------------------------------------------------------------------------
-- UserLinks
--------------------------------------------------------------------------------

newtype UserLinks = UserLinks
  { userLinksSelf :: URL
  -- ^ The link to use to retrieve the user information
  } deriving (Show, Eq, Generic)

instance ToFromJSON UserLinks

--------------------------------------------------------------------------------
-- Mention
--------------------------------------------------------------------------------

data MentionItem = MentionItem
  { mentionItemId           :: Int
  , mentionItemVersion      :: Text
  , mentionItemName         :: Text
  , mentionItemMentionName  :: Text
  , mentionItemMentionLinks :: UserLinks
  } deriving (Show, Eq, Generic)

instance ToFromJSON MentionItem

makeFields ''MentionItem

--------------------------------------------------------------------------------
-- FileInfo
--------------------------------------------------------------------------------

data FileInfo = FileInfo
  { fileInfoName     :: Text
  -- ^ The name of the file
  , fileInfoSize     :: Int
  -- ^ The size of the file in bytes
  , fileInfoThumbUrl :: Maybe URL
  -- ^ The URL of the thumbnail if it exists
  , fileInfoUrl      :: URL
  -- ^ The URL of the file
  } deriving (Show, Eq, Generic)

makeFields ''FileInfo

instance ToFromJSON FileInfo

--------------------------------------------------------------------------------
-- Sender
--------------------------------------------------------------------------------

-- data Sender = Sender
--   { senderId          :: Int
--   -- ^ The user ID
--   , senderLinks       :: UserLinks
--   -- ^ URLs to retrieve user information
--   , senderMentionName :: Text
--   -- ^ User's @mention name
--   , senderName        :: Text
--   -- ^ The display user name
--   , senderVersion     :: Text
--   -- ^ An etag-like random version string
--   } deriving (Eq, Show, Generic)
--
-- makeFields ''Sender

data family Sender (e :: RoomEvent)

data instance Sender 'RoomMessage
  = SimpleMessageSender Text
  | MessageSender
    { senderId          :: Int
    -- ^ The user ID
    , senderLinks       :: UserLinks
    -- ^ URLs to retrieve user information
    , senderMentionName :: Text
    -- ^ User's @mention name
    , senderName        :: Text
    -- ^ The display user name
    , senderVersion     :: Text
    -- ^ An etag-like random version string
    } deriving (Show, Eq, Generic)

instance ToFromJSON (Sender 'RoomMessage)

data instance Sender 'RoomTopicChange
  = SimpleTopicChangeSender
    { topicChangeSenderName :: Text
    }
  | TopicChangeSender
    { topicChangeSenderId          :: Int
    -- ^ The user ID
    , topicChangeSenderLinks       :: UserLinks
    -- ^ URLs to retrieve user information
    , topicChangeSenderMentionName :: Text
    -- ^ User's @mention name
    , topicChangeSenderName        :: Text
    -- ^ The display user name
    , topicChangeSenderVersion     :: Text
    -- ^ An etag-like random version string
    }
  | ExtendedTopicChangeSender
    { topicChangeSenderAllowedScopes :: [Text] --TODO [ApiScope]?
    -- ^ List of possible scopes that are allowed for requested tokens
    , topicChangeSenderId            :: Int
    -- ^ ID of the client
    , topicChangeSenderLinks         :: UserLinks
    -- ^ URLs to retrive the full oauth client information
    , topicChangeSenderName          :: Text --TODO Maybe Text
    -- ^ The name of the client, if any
    , topicChangeSenderRoom          :: Maybe Room
    -- ^ The room, if specified, that this client is restricted to.
    }
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- MessageLink
--------------------------------------------------------------------------------

data Image = Image
  { imageImage :: URL
  , imageName  :: Text
  } deriving (Show, Eq, Generic)

instance ToFromJSON Image

-- TODO
data MessageLink = MessageLink
  { messageLinkImage         :: Maybe Image
  -- , messageLinkLink          :: Maybe
  -- , messageLinkTwitterStatus :: Maybe
  -- , messageLinkTwitterUser   :: Maybe
  -- , messageLinkType          :: Maybe
  -- , messageLinkUrl           :: Maybe
  -- , messageLinkVideo         :: Maybe
  } deriving (Show, Eq, Generic)

instance ToFromJSON MessageLink

--------------------------------------------------------------------------------
-- Message
--------------------------------------------------------------------------------

data MessageFormat = HtmlFormat | TextFormat
  deriving (Show, Eq, Ord, Generic)

instance AsText MessageFormat where
  enc HtmlFormat = "html"
  enc TextFormat = "text"
  dec "html" = Just HtmlFormat
  dec "text" = Just TextFormat
  dec _      = Nothing

instance ToJSON MessageFormat where
  toJSON = AsText.toJSON

instance FromJSON MessageFormat where
  parseJSON = AsText.parseJSON

instance IsString MessageFormat where
  fromString = AsText.fromString

instance Default MessageFormat where
  def = HtmlFormat

data MessageType
  = TypeMessage | TypeGuestAccess | TypeTopic | TypeNotification
  deriving (Show, Eq, Generic)

instance AsText MessageType where
  enc TypeMessage      = "message"
  enc TypeGuestAccess  = "guest_access"
  enc TypeTopic        = "topic"
  enc TypeNotification = "notification"
  dec "message"      = Just TypeMessage
  dec "guest_access" = Just TypeGuestAccess
  dec "topic"        = Just TypeTopic
  dec "notification" = Just TypeNotification
  dec _              = Nothing

instance ToJSON MessageType where
  toJSON = AsText.toJSON

instance FromJSON MessageType where
  parseJSON = AsText.parseJSON

data Color = Yellow | Green | Red | Purple | Gray
  deriving (Show, Eq, Generic)

instance Default Color where def = Yellow

instance ToFromJSON Color

data family Message (e :: RoomEvent)

data instance Message 'RoomMessage =
  Message
  { messageDate         :: UTCTime
  -- ^ The date the message was sent in ISO-8601 format.
  , messageFile         :: Maybe FileInfo
  -- ^ The file URL attached to the message, if specified.
  , messageColor        :: Maybe Color
  -- ^ Background color for message. Defaults to Yellow.
  , messageFrom         :: Sender 'RoomMessage
  -- ^ The user that sent the message.
  , messageId           :: UUID
  -- ^ The internal unique id of the message
  , messageMentions     :: [MentionItem]
  -- ^ A list of mentioned users in this message.
  , messageMessage      :: Text
  -- ^ The message sent.
  , messageMessageLinks :: Maybe [MessageLink]
  -- ^ A list of links extracted from the message and some extended data
  -- (currently only extracts the last link from the message).
  , messageType         :: MessageType
  -- ^ The type of message being returned.
  } deriving (Show, Eq, Generic)

instance ToFromJSON (Message 'RoomMessage)

data instance Message 'RoomNotification = NotificationMessage
  { notificationMessageColor         :: Maybe Color
  -- ^ Background color for message. Defaults to Yellow.
  , notificationMessageDate          :: UTCTime
  -- ^ The date the message was sent in ISO-8601 format
  , notificationMessageFrom          :: Text
  -- ^ The integration name that sent the message. May be null.
  -- TODO Maybe?
  , notificationMessageId            :: UUID
  , notificationMessageMentions      :: [MentionItem]
  -- ^ A list of mentioned users in this message.
  , notificationMessageMessage       :: Text
  -- ^ The message sent.
  , notificationMessageMessageFormat :: MessageFormat
  -- ^ Determines how the message is treated by our server and rendered inside HipChat applications.
  , notificationMessageType          :: MessageType
  }
  deriving (Show, Eq, Generic)

instance ToFromJSON (Message 'RoomNotification)

--------------------------------------------------------------------------------
-- RoomLinks
--------------------------------------------------------------------------------

data RoomLinks = RoomLinks
  { roomLinksMembers      :: Maybe URL
  -- ^ The URL to use to retrieve members for this room.
  -- Only available for private rooms.
  -- Note this link will be made available to public room from Dec 1, 2016.
  , roomLinksParticipants :: URL
  -- ^ The URL to use to retrieve participants for this room.
  , roomLinksSelf         :: URL
  -- ^ The URL to use to retrieve the full room information.
  , roomLinksWebhooks     :: URL
  -- ^ The URL to use to retrieve participants for this room.
  } deriving (Eq, Show, Generic)

instance ToFromJSON RoomLinks

--------------------------------------------------------------------------------
-- Room
--------------------------------------------------------------------------------


data Privacy = Private | Public
  deriving (Show, Eq, Ord, Bounded, Enum, Ix, Generic)

instance ToFromJSON Privacy

data Room = Room
  { roomId         :: Int
  -- ^ ID of the room
  , roomIsArchived :: Bool
  -- ^ Whether or not this room is archived
  , roomLinks      :: RoomLinks
  -- ^ URLs to retrieve room information
  , roomName       :: Text
  -- ^ Name of the room
  , roomPrivacy    :: Privacy
  -- ^ Privacy setting
  , roomVersion    :: Text
  -- ^ An etag-like random version string
  } deriving (Eq, Show, Generic)

instance ToFromJSON Room

makeFields ''Room

--------------------------------------------------------------------------------
-- Notification
--------------------------------------------------------------------------------

data Notification (e :: RoomEvent) = Notification
  { notificationEvent         :: RoomEvent
  -- ^ The webhook event name
  , notificationItem          :: Item e
  -- ^ The event data
  , notificationOauthClientId :: Maybe Text
  -- ^ The oauth client id that registered this webhook, if applicable.
  , notificationWebhookId     :: Int
  -- ^ The unique identifier for this webhook registration.
  }

--------------------------------------------------------------------------------
-- Item
--------------------------------------------------------------------------------

type family Item (e :: RoomEvent) where
  Item 'RoomArchived     = RoomArchivedItem
  Item 'RoomCreated      = RoomCreatedItem
  Item 'RoomDeleted      = RoomDeletedItem
  Item 'RoomEnter        = RoomEnterItem
  Item 'RoomExit         = RoomExitItem
  Item 'RoomFileUpload   = RoomFileUploadItem
  Item 'RoomMessage      = RoomMessageItem
  Item 'RoomNotification = RoomNotificationItem
  Item 'RoomTopicChange  = RoomTopicChangeItem
  Item 'RoomUnarchived   = RoomUnarchivedItem

data RoomArchivedItem   = RoomArchivedItem
data RoomCreatedItem    = RoomCreatedItem
data RoomDeletedItem    = RoomDeletedItem
data RoomEnterItem      = RoomEnterItem
data RoomExitItem       = RoomExitItem
data RoomFileUploadItem = RoomFileUploadItem

data RoomMessageItem = RoomMessageItem
  { roomMessageItemMessage :: Message 'RoomMessage
  , roomMessageItemRoom    :: Room
  } deriving (Show, Eq, Generic)

instance ToFromJSON RoomMessageItem

data RoomNotificationItem = RoomNotificationItem
  { roomNotificationItemMessage :: Message 'RoomNotification
  , roomNotificationItemRoom    :: Room
  } deriving (Show, Eq, Generic)

instance ToFromJSON RoomNotificationItem

data RoomTopicChangeItem = RoomTopicChangeItem
  { roomTopicChangeItemRoom   :: Room
  -- ^ The room whose topic has changed.
  , roomTopicChangeItemSender :: Sender 'RoomTopicChange
  -- ^ The member that changed the topic.
  , roomTopicChangeItemTopic  :: Text
  -- ^ The new topic.
  }

data RoomUnarchivedItem = RoomUnarchivedItem

--------------------------------------------------------------------------------
-- RoomNotificationResp
--------------------------------------------------------------------------------

data RoomNotificationResp = RoomNotificationResp
  { roomNotificationEvent         :: Text
  , roomNotificationWebhookId     :: Int
  , roomNotificationItem          :: RoomNotificationItem
  , roomNotificationOauthClientId :: Maybe UUID -- TODO make newtype wrapper
  } deriving (Show, Eq, Generic)

instance ToFromJSON RoomNotificationResp

makeFields ''RoomNotificationItem
makeFields ''RoomNotificationResp

--------------------------------------------------------------------------------
-- RoomMessageResp
--------------------------------------------------------------------------------

data RoomMessageResp = RoomMessageResp
  { roomMessageEvent         :: Text
  , roomMessageWebhookId     :: Int
  , roomMessageItem          :: RoomMessageItem
  , roomMessageOauthClientId :: Maybe UUID -- TODO make newtype wrapper
  } deriving (Show, Eq, Generic)

instance ToFromJSON RoomMessageResp

makeLensesWith camelCaseFields ''RoomMessageItem
makeLensesWith camelCaseFields ''RoomMessageResp

--------------------------------------------------------------------------------
-- WebhookResp
--
--------------------------------------------------------------------------------

-- TODO
data WebhookResp (e :: RoomEvent) = WebhookResp
  { webhookRespEvent  :: Text
  , webhookRespItem   :: Item e
  , webhookRespSender :: Sender e
  }

-- instance ToFromJSON (WebhookResp RoomMessage)

-- makeFields (''WebhookResp 'RoomMessage)
{-
"\\"{\
    \\"event\\": \\"room_message\\",\
    \\"webhook_id\\": 17775373,\
    \\"item\\": {\
        \\"room\\": {\
            \\"is_archived\\": false,\
            \\"privacy\\": \\"private\\",\
            \\"name\\": \\"MarkTesting\\",\
            \\"version\\": \\"UDY9AI3A\\",\
            \\"id\\": 3856578,\
            \\"links\\": {\
                \\"members\\": \\"https://api.hipchat.com/v2/room/3856578/member\\",\
                \\"participants\\": \\"https://api.hipchat.com/v2/room/3856578/participant\\",\
                \\"self\\": \\"https://api.hipchat.com/v2/room/3856578\\",\
                \\"webhooks\\": \\"https://api.hipchat.com/v2/room/3856578/webhook\\"\
            }\
        },\
        \\"message\\": {\
            \\"from\\": {\
                \\"mention_name\\": \\"MarkHopkins\\",\
                \\"name\\": \\"Mark Hopkins\\",\
                \\"version\\": \\"FPUR1NDI\\",\
                \\"id\\": 4979430,\
                \\"links\\": {\
                    \\"self\\": \\"https://api.hipchat.com/v2/user/4979430\\"\
                }\
            },\
            \\"date\\": \\"2017-05-20T13:56:46.750428+00:00\\",\
            \\"id\\": \\"f74a26c3-f54e-499d-b3b6-28fc28c932bb\\",\
            \\"type\\": \\"message\\",\
            \\"message\\": \\"/hey @MarkHopkins testing\\",\
            \\"mentions\\": [\
                {\
                    \\"mention_name\\": \\"MarkHopkins\\",\
                    \\"name\\": \\"Mark Hopkins\\",\
                    \\"version\\": \\"FPUR1NDI\\",\
                    \\"id\\": 4979430,\
                    \\"links\\": {\
                        \\"self\\": \\"https://api.hipchat.com/v2/user/4979430\\"\
                    }\
                }\
            ]\
        }\
    },\
    \\"oauth_client_id\\": \\"f3a628d0-be81-461a-b025-41604d982310\\"\
}\\""
-}
