--------------------------------------------------------------------------------
-- |
-- Module: HipChat.Types
-- Description: Common types used in both the HipChat REST API and Add-on API.
--
--------------------------------------------------------------------------------

module HipChat.Types where

import           Data.Text         (Text)
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
