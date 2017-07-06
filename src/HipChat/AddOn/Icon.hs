{-# LANGUAGE OverloadedStrings #-}

module HipChat.AddOn.Icon (Icon(..)) where

import           Data.Aeson
import           Data.Aeson.Types

import           HipChat.Types.URL

data Icon
  = Icon { iconUrl :: URL }
  | IconHighRes { iconUrl :: URL, iconUrl2x :: URL }
  deriving (Show, Eq)

instance ToJSON Icon where
  toJSON (Icon url)             = toJSON url
  toJSON (IconHighRes url url2) = object ["url" .= url, "url@2x" .= url2]

instance FromJSON Icon where
  parseJSON s@(String _) = Icon <$> parseJSON s
  parseJSON (Object v)   = IconHighRes <$> v .: "url" <*> v .: "url@2x"
  parseJSON other        = typeMismatch "String or Object" other
