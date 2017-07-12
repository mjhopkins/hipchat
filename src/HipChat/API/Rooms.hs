{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.API.Rooms
-- Description: HipChat REST API for interacting with rooms
--
--------------------------------------------------------------------------------

module  HipChat.API.Rooms where

import           HipChat.Auth.Token (Token, TokenAuth)
import           HipChat.Types      (Room)

import           Data.Proxy         (Proxy (..))
import           Data.Text          (Text)
import           Servant.API        ((:>), Capture, JSON, PlainText, Post,
                                     ReqBody)
import           Servant.Client     (ClientM, client)
type RoomsAPI =
  TokenAuth :> "v2" :> Capture "room" Room :> "notification" :> ReqBody '[PlainText] Text :> Post '[JSON] ()
  -- ^ Send a message to a room. This resource accepts three different content-types:
  --
  --   * application/x-www-form-urlencoded - If you send as a form-encoded POST, the form fields will be mapped to their corresponding JSON properties.
  --
  --   * text/plain - If you send with the text/plain content-type, the payload will be treated as a plain text message with a default background color of yellow and the notify flag set to false.
  --
  --   * text/html - If you send with the text/html content-type, the payload will be treated as an HTML message with a default background color of yellow and the notify flag set to false.
  --
  --   * application/json - If you send with the application/json content-type, the payload will be treated as JSON with the expected format described in the Requested Body section below.
  --
  -- Token needs to have send_notification scope and client type group client, room client or user.

sendPlainTextRoomNotification :: Token -> Room -> Text -> ClientM ()
sendPlainTextRoomNotification = client (Proxy :: Proxy RoomsAPI)
