{-# LANGUAGE OverloadedStrings #-}

module HipChat.AddOn.Key where

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.AddOn.Key
--
-- length-restricted key type (1-40 chars), should be unique within some context.
--------------------------------------------------------------------------------

import           Control.Lens
import           Control.Lens.AsText (AsText, parse)
import qualified Control.Lens.AsText as AsText
import           Data.Aeson
import           Data.Monoid
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show)

instance AsText Key where
  parse = prism' enc dec
    where
      enc = unKey
      dec t = if l >= 1 && l <= 40
              then Just $ Key t
              else Nothing
              where l = T.length t

instance IsString Key where
  fromString = AsText.fromString

instance ToJSON Key where
  toJSON (Key x) = toJSON x

instance FromJSON Key where
  parseJSON x = do
    rawKey <- parseJSON x
    case rawKey ^? parse of
      Nothing -> fail $ "Invalid key length, expected 1..40, got: " <> show (T.length rawKey)
      Just k  -> return k
