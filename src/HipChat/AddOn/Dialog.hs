{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.AddOn.Dialog
--
-- https://www.hipchat.com/docs/apiv2/dialogs
-- https://developer.atlassian.com/hipchat/guide/dialog
--------------------------------------------------------------------------------

module HipChat.AddOn.Dialog where

import           HipChat.AddOn.Types  (AuthenticationMethod, Name)
import           HipChat.Types        (Key, URL)
import           HipChat.Util         (ToFromJSON)

import           Control.Applicative  ((<|>))
import           Control.Lens.AsText  (AsText)
import qualified Control.Lens.AsText  as AsText
import           Control.Lens.TH      (camelCaseFields, makeLensesWith)
import           Data.Aeson           (FromJSON, ToJSON, parseJSON, toJSON)
import qualified Data.Attoparsec.Text as A
import           Data.Default         (Default, def)
import           Data.String          (IsString, fromString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

--------------------------------------------------------------------------------
-- DialogStyle
--------------------------------------------------------------------------------

data DialogStyle = Normal | Warning
  deriving (Show, Eq, Ord)

instance AsText DialogStyle where
  enc Normal  = "normal"
  enc Warning = "warning"

  dec "normal"  = Just Normal
  dec "warning" = Just Warning
  dec _         = Nothing

instance Default DialogStyle where
  def = Normal

instance ToJSON DialogStyle where
  toJSON = AsText.toJSON

instance FromJSON DialogStyle where
  parseJSON = AsText.parseJSON

--------------------------------------------------------------------------------
-- DialogAction
--------------------------------------------------------------------------------

data DialogAction = DialogAction
  { dialogActionName    :: Name
  -- ^ The localizable name of the button
  , dialogActionEnabled :: Bool
  -- ^ Whether button is disabled or enabled (default 'true')
  , dialogActionKey     :: Maybe Key
  -- ^ A key that can be used to register an event listener.
  -- Default 'primary-action-selected' for the primary action, none for secondary actions.
  } deriving (Show, Eq, Generic)

instance ToFromJSON DialogAction

--------------------------------------------------------------------------------
-- DialogOptions
--------------------------------------------------------------------------------

data Size = Pixels Int | Percentage Int
  deriving (Eq, Generic)

instance Show Size where
  show (Pixels i)     = show i ++ "px"
  show (Percentage i) = show i ++ "%"

instance IsString Size where
  fromString =
    either (error . ("Unable to parse size: " ++)) id
    . A.parseOnly sizeParser
    . T.pack

sizeParser :: A.Parser Size
sizeParser = Pixels <$> A.decimal <* "px" <* A.endOfInput
         <|> Percentage <$> A.decimal <* "%" <* A.endOfInput

data DialogSize = DialogSize
  { dialogSizeHeight :: Text
  -- ^ The height of the dialog, either in pixels ('px') or as a percentage ('%').
  , dialogSizeWidth  :: Text
  -- ^ The width of the dialog, either in pixels ('px') or as a percentage ('%').
  } deriving (Show, Eq, Generic)

instance ToFromJSON DialogSize

newtype DialogFilter = DialogFilter
  { dialogFilterPlaceholder :: Name
  -- ^ The placeholder text for the filter box.
  } deriving (Show, Eq, Generic)

instance ToFromJSON DialogFilter

data DialogOptions = DialogOptions
  { dialogOptionsStyle            :: Maybe DialogStyle
  -- ^ The dialog style (default is 'normal')
  , dialogOptionsPrimaryAction    :: Maybe DialogAction
  -- ^ The primary action of the dialog, rendered as a primary button.
  , dialogOptionsSecondaryActions :: Maybe [DialogAction]
  -- ^ The secondary actions of the dialog, rendered as link buttons.
  -- If not specified, a default "Close" button will be rendered.
  -- You can declare an empty array for no secondary actions.
  , dialogOptionsSize             :: Maybe DialogSize
  -- ^ The size of the dialog
  , dialogOptionsHint             :: Maybe Name
  -- ^ The dialog hint, displayed in the left side of the bottom bar
  , dialogOptionsFilter           :: Maybe DialogFilter
  -- ^ If present, adds a filter box in the dialog header
  } deriving (Show, Eq, Generic)

instance ToFromJSON DialogOptions

defaultDialogOptions :: DialogOptions
defaultDialogOptions = DialogOptions Nothing Nothing Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------
-- Dialog
--------------------------------------------------------------------------------

data Dialog = Dialog
  { dialogAuthentication :: Maybe AuthenticationMethod
  -- ^ The authentication method for this dialog. Defaults to JWT.
  , dialogKey            :: Key
  -- ^ Unique key (in the context of the integration) to identify this dialog.
  , dialogOptions        :: Maybe DialogOptions
  -- ^ Dialog options
  , dialogTitle          :: Name
  -- ^ The dialog title
  , dialogUrl            :: URL
  -- ^ The url where the dialog content is hosted.
  } deriving (Show, Eq, Generic)

mkDialog :: Key -> Name -> URL -> Dialog
mkDialog k = Dialog Nothing k Nothing

instance ToFromJSON Dialog

makeLensesWith camelCaseFields ''Dialog
