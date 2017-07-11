{-# LANGUAGE OverloadedStrings #-}

module HipChat.AddOn.Name where

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.AddOn.Name
--
-- Common name type
--------------------------------------------------------------------------------

import           Data.Aeson  (FromJSON, ToJSON, object, parseJSON, toJSON,
                              withObject, (.:), (.:?), (.=))
import           Data.Monoid ((<>))
import           Data.Text   (Text)

data Name = Name
  { nameValue :: Text       -- ^ The default text. Valid length range: 1 - 100.
  , nameI18n  :: Maybe Text -- ^ The optional localization key, used to look up the localized value. Valid length range: 1 - 40.
  } deriving (Show, Eq)

instance ToJSON Name where
  toJSON (Name value i18n) =
    object $ [ "value" .= value ] <> maybe [] (\x -> [ "i18n" .= x ]) i18n

instance FromJSON Name where
  parseJSON = withObject "object" $ \o -> Name <$> o .: "value" <*> o .:? "i18n"

--TODO actually check lengths
