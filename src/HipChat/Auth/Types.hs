{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

module HipChat.Auth.Types where

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.Auth.Types
--
-- Types involved in authentication and authorization, e.g. OAuth token requests
--
--------------------------------------------------------------------------------

import           Control.Lens                hiding ((.=))
import           Control.Lens.AsText         (AsText, parse)
import qualified Control.Lens.AsText         as AsText

import           Data.Aeson                  (FromJSON (parseJSON),
                                              ToJSON (toJSON), Value (String),
                                              object, withObject, (.:), (.:?),
                                              (.=))
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           GHC.Generics                (Generic)

import           Web.FormUrlEncoded          (FromForm (fromForm),
                                              ToForm (toForm), parseUnique)
import           Web.HttpApiData             (FromHttpApiData (parseUrlPiece),
                                              ToHttpApiData (toUrlPiece),
                                              parseQueryParam, toQueryParam)

--------------------------------------------------------------------------------
-- APIScope
--------------------------------------------------------------------------------

data APIScope
  = AdminGroup         -- ^ Perform group administrative tasks
  | AdminRoom          -- ^ Perform room administrative tasks
  | ManageRooms        -- ^ Create, update, and remove rooms
  | SendMessage        -- ^ Send private one-on-one messages
  | SendNotification   -- ^ Send room notifications
  | ViewGroup          -- ^ View users, rooms, and other group information
  | ViewMessages       -- ^ View messages from chat rooms and private chats you have access to
  | ViewRoom           -- ^ View room information and participants, but not history
  deriving (Show, Read, Eq, Generic)

instance AsText APIScope where
  parse = prism' enc dec where
    enc = \case
      AdminGroup          -> "admin_group"
      AdminRoom           -> "admin_room"
      ManageRooms         -> "manage_rooms"
      SendMessage         -> "send_message"
      SendNotification    -> "send_notification"
      ViewGroup           -> "view_group"
      ViewMessages        -> "view_messages"
      ViewRoom            -> "view_room"
    dec = \case
      "admin_group"       -> Just AdminGroup
      "admin_room"        -> Just AdminRoom
      "manage_rooms"      -> Just ManageRooms
      "send_message"      -> Just SendMessage
      "send_notification" -> Just SendNotification
      "view_group"        -> Just ViewGroup
      "view_messages"     -> Just ViewMessages
      "view_room"         -> Just ViewRoom
      _                   -> Nothing

instance ToHttpApiData APIScope where
  toUrlPiece = AsText.toUrlPiece

instance FromHttpApiData APIScope where
  parseUrlPiece = AsText.parseUrlPiece

instance ToJSON APIScope where
  toJSON = AsText.toJSON

instance FromJSON APIScope where
  parseJSON = AsText.parseJSON

instance IsString APIScope where
  fromString = AsText.fromString

--------------------------------------------------------------------------------
-- GrantType
--------------------------------------------------------------------------------

data GrantType
  = AuthorizationCode
  | RefreshToken
  | Password
  | ClientCredentials
  | Personal
  | RoomNotification
  | Internal -- ^ allowed, but not documented
    deriving (Eq, Show, Read, Generic)

instance AsText GrantType where
  enc = \case
    AuthorizationCode -> "authorization_code"
    RefreshToken      -> "refresh_token"
    Password          -> "password"
    ClientCredentials -> "client_credentials"
    Personal          -> "personal"
    RoomNotification  -> "room_notification"
    Internal          -> "internal"
  dec = \case
   "authorization_code" -> Just AuthorizationCode
   "refresh_token"      -> Just RefreshToken
   "password"           -> Just Password
   "client_credentials" -> Just ClientCredentials
   "personal"           -> Just Personal
   "room_notification"  -> Just RoomNotification
   "internal"           -> Just Internal
   _                    -> Nothing

instance ToHttpApiData GrantType where
  toUrlPiece = AsText.toUrlPiece

instance FromHttpApiData GrantType where
  parseUrlPiece = AsText.parseUrlPiece

instance ToJSON GrantType where
  toJSON = AsText.toJSON

instance FromJSON GrantType where
  parseJSON = AsText.parseJSON

instance IsString GrantType where
  fromString = AsText.fromString

--------------------------------------------------------------------------------
-- Token request
--------------------------------------------------------------------------------

data TokenReq = TokenReq
  { tokenReqGrantType    :: GrantType  -- ^ The type of grant request.
  , tokenReqScope        :: [APIScope] -- ^ A space-delimited list of scopes that is requested.
  , tokenReqUsername     :: Maybe Text -- ^ The user name to generate a token on behalf of. Only valid in the 'password' and 'client_credentials' grants.
  , tokenReqUserId       :: Maybe Text -- int?
                                           -- ^ The id of the user the token is acting on behalf of. Only valid in the 'authorization_code' and 'refresh_token' grants.
  , tokenReqCode         :: Maybe Text -- ^ The authorization code to exchange for an access token. Only valid in the 'authorization_code' grant.
  , tokenReqClientName   :: Maybe Text -- ^ The name of the public oauth client retrieving a token for. Only valid in the 'authorization_code' and 'refresh_token' grants.
  , tokenReqRedirectUrl  :: Maybe Text -- ^ The URL that was used to generate an authorization code, and it must match that value. Only valid in the 'authorization_code' grant.
  , tokenReqPassword     :: Maybe Text -- ^ The user's password to use for authentication when creating a token. Only valid in the 'password' grant.
  , tokenReqGroupId      :: Maybe Text -- ^ The name of the group to which the related user belongs. Only valid in the 'authorization_code' and 'refresh_token' grants.
  , tokenReqRefreshToken :: Maybe Text -- ^ The refresh token to use to generate a new access token. Only valid in the 'refresh_token' grant.
  } deriving (Eq, Show, Read, Generic)

makeLensesWith camelCaseFields ''TokenReq

tokenReq :: GrantType -> TokenReq
tokenReq gt = TokenReq gt [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

addOnTokenReq :: [APIScope] -> TokenReq
addOnTokenReq scopes = tokenReq ClientCredentials & scope .~ scopes

instance ToForm TokenReq where
  toForm req =
    [ ("grant_type", toQueryParam (req ^. grantType))
    , ("scope", T.unwords $ toQueryParam <$> req ^. scope) -- TODO nonempty?
    , foldMap (("username",)      . toQueryParam)  (req ^. username)
    , foldMap (("user_id",)       . toQueryParam)  (req ^. userId)
    , foldMap (("code",)          . toQueryParam)  (req ^. code)
    , foldMap (("client_name",)   . toQueryParam)  (req ^. clientName)
    , foldMap (("redirect_url",)  . toQueryParam)  (req ^. redirectUrl)
    , foldMap (("password",)      . toQueryParam)  (req ^. password)
    , foldMap (("group_id",)      . toQueryParam)  (req ^. groupId)
    , foldMap (("refresh_token",) . toQueryParam)  (req ^. refreshToken)
    ]

instance FromForm TokenReq where
  fromForm f = TokenReq
    <$> parseUnique "grant_type" f
    <*> (parseUnique "scope" f >>= traverse parseQueryParam . T.words)
    <*> parseUnique "username" f
    <*> parseUnique "user_id" f
    <*> parseUnique "code" f
    <*> parseUnique "client_name" f
    <*> parseUnique "redirect_url" f
    <*> parseUnique "password" f
    <*> parseUnique "group_id" f
    <*> parseUnique "refresh_token" f

--------------------------------------------------------------------------------
-- Token response
--------------------------------------------------------------------------------

data TokenResp = TokenResp
  { tokenRespAccessToken  :: Text          -- ^ The generated access token to use to authenticate future requests.
  , tokenRespExpiresIn    :: Int           -- ^ The number of seconds this token will be valid for.
  , tokenRespGroupId      :: Int           -- ^ The HipChat group ID this token belongs to
  , tokenRespGroupName    :: Text          -- ^ The HipChat group name this token belongs to.
  , tokenRespTokenType    :: Text          -- ^ The type of token returned. Always 'bearer'.
  , tokenRespScope        :: [APIScope]    -- ^ A space-delimited list of scopes that this token is allowed to use.
  , tokenRespRefreshToken :: Maybe Text    -- ^ The generated refresh token to use when requesting new access tokens.
  } deriving (Eq, Show, Read, Generic)

makeLensesWith camelCaseFields ''TokenResp

instance ToJSON TokenResp where
  toJSON t = object
    [ "access_token"  .= (t ^. accessToken)
    , "expires_in"    .= (t ^. expiresIn)
    , "group_id"      .= (t ^. groupId)
    , "group_name"    .= (t ^. groupName)
    , "token_type"    .= (t ^. tokenType)
    , "token_scope"   .= T.unwords (toQueryParam <$> t ^. scope)
    , "refresh_token" .= (t ^. refreshToken)
    ]

instance FromJSON TokenResp where
  parseJSON = withObject "object" $ \o -> TokenResp
    <$> o .: "access_token"
    <*> o .: "expires_in"
    <*> o .: "group_id"
    <*> o .: "group_name"
    <*> o .: "token_type"
    <*> (o .: "scope" >>= traverse (parseJSON . String) . T.words)
    <*> o .:? "refresh_token"
