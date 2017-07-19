{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module HipChat.AddOn.Types.Name where

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.AddOn.Types.Name
--
-- Localizable names
--------------------------------------------------------------------------------

import           Control.Lens.TH (camelCaseFields, makeLensesWith)
import           Data.Aeson      (FromJSON, ToJSON, object, parseJSON, toJSON,
                                  withObject, (.:), (.:?), (.=))
import           Data.Monoid     ((<>))
import           Data.String     (IsString, fromString)
import           Data.Text       (Text)
import qualified Data.Text       as T

data Name = Name
  { nameValue :: Text
  -- ^ The default text. Valid length range: 1 - 100.
  , nameI18n  :: Maybe Text
  -- ^ The optional localization key, used to look up the localized value. Valid length range: 1 - 40.
  } deriving (Show, Eq)

instance ToJSON Name where
  toJSON (Name value i18n) =
    object $ [ "value" .= value ] <> maybe [] (\x -> [ "i18n" .= x ]) i18n

instance FromJSON Name where
  parseJSON = withObject "object" $ \o -> Name <$> o .: "value" <*> o .:? "i18n"

--TODO actually check lengths

instance IsString Name where
  fromString s = Name (T.pack s) Nothing

makeLensesWith camelCaseFields ''Name
