{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HipChat.AddOn.Types.RoomEvent where

import           Control.Lens.AsText (AsText)
import qualified Control.Lens.AsText as AsText
import           Data.Aeson          (FromJSON, ToJSON, parseJSON, toJSON)

data RoomEvent
  = RoomArchived
  | RoomCreated
  | RoomDeleted
  | RoomEnter
  | RoomExit
  | RoomFileUpload
  | RoomMessage
  | RoomNotification
  | RoomTopicChange
  | RoomUnarchived
  deriving (Show, Eq)

instance AsText RoomEvent where
  enc = \case
    RoomArchived     -> "room_archived"
    RoomCreated      -> "room_created"
    RoomDeleted      -> "room_deleted"
    RoomEnter        -> "room_enter"
    RoomExit         -> "room_exit"
    RoomFileUpload   -> "room_file_upload"
    RoomMessage      -> "room_message"
    RoomNotification -> "room_notification"
    RoomTopicChange  -> "room_topic_change"
    RoomUnarchived   -> "room_unarchived"
  dec = \case
    "room_archived"     -> Just RoomArchived
    "room_created"      -> Just RoomCreated
    "room_deleted"      -> Just RoomDeleted
    "room_enter"        -> Just RoomEnter
    "room_exit"         -> Just RoomExit
    "room_file_upload"  -> Just RoomFileUpload
    "room_message"      -> Just RoomMessage
    "room_notification" -> Just RoomNotification
    "room_topic_change" -> Just RoomTopicChange
    "room_unarchived"   -> Just RoomUnarchived
    _                   -> Nothing

instance ToJSON RoomEvent where
  toJSON = AsText.toJSON

instance FromJSON RoomEvent where
  parseJSON = AsText.parseJSON
