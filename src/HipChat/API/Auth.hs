{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.API.Auth
-- Description: OAuth authenication with the HipChat server
--
--------------------------------------------------------------------------------

module HipChat.API.Auth where

import           Data.Monoid        ((<>))
import           Data.Proxy         (Proxy (..))
import           Data.String        (IsString (..))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Servant.API        ((:>))
import           Servant.Client     (Client, HasClient (..))
import           Servant.Common.Req (Req, addHeader)

-- | OAuth Token
newtype Token = Token Text
  deriving (Show, Eq)

instance IsString Token where
  fromString = Token . T.pack

data TokenAuth

instance HasClient api => HasClient (TokenAuth :> api) where
  type Client (TokenAuth :> api) = Token -> Client api

  clientWithRoute _ req tok =
    clientWithRoute (Proxy :: Proxy api) (addToken tok req)

addToken :: Token -> Req -> Req
addToken (Token t) = addHeader "Authorization" ("Bearer " <> t)
