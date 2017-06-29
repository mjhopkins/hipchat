--------------------------------------------------------------------------------
-- |
-- Module: HipChat.Types
-- Description: Common types used in both the HipChat REST API and Add-on API.
--
--------------------------------------------------------------------------------

module HipChat.Types where

import           Data.Text (Text)

-- | The id or url-encoded name of the room
-- Valid length range: 1 - 100.
type RoomId = Text
