{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

--------------------------------------------------------------------------------
-- |
-- Module HipChat.AddOn.Glance
--
-- Glances are a way of displaying important information about an integration in
-- a HipChat room. They are useful for showing aggregate information and
-- alleviate spamming a room with multiple separate messages.
-- The content and format for a glance is fetched dynamically from the
-- integration server based on the queryUrl parameter.
--
-- https://www.hipchat.com/docs/apiv2/glances
-- https://developer.atlassian.com/hipchat/guide/glances
--------------------------------------------------------------------------------

module HipChat.AddOn.Glance where

import           HipChat.AddOn.Types (Icon, Name)
import           HipChat.Types       (Key, URL)
import           HipChat.Util        (ToFromJSON, unwrappedUnaryRecords,
                                      variant)

import           Control.Lens.AsText (AsText, dec, enc)
import qualified Control.Lens.AsText as AsText
import           Control.Lens.TH     (camelCaseFields, makeLensesWith)
import           Data.Aeson          (FromJSON, Object, ToJSON, parseJSON,
                                      toJSON)
import           Data.String         (IsString, fromString)
import           GHC.Generics

--------------------------------------------------------------------------------
-- GlanceTarget
--------------------------------------------------------------------------------

data GlanceTarget
  = SimpleGlanceTarget
    { glanceTargetKey :: Key
    -- ^ The key of a dialog, glance or web panel that should be opened in
    -- response to this action. Valid length range: 1 - 40.
    }
  | CompoundGlanceTarget
    { glanceTargetKey     :: Key
    -- ^ The key of a dialog, glance or web panel that should be opened in
    -- response to this action. Valid length range: 1 - 40.
    , glanceTargetOptions :: Maybe Object
    -- ^ An object containing options which vary based on the type of target.
    }
    deriving (Show, Eq, Generic)

instance ToFromJSON GlanceTarget where
  variant = unwrappedUnaryRecords

--------------------------------------------------------------------------------
-- GlanceCondition
--------------------------------------------------------------------------------

data Condition
  = RoomIsPublic
  | UserIsAdmin
  | UserIsGuest
  | UserIsRoomOwner
  | GlanceMatches
  deriving (Eq, Show, Generic)

instance AsText Condition where
  enc RoomIsPublic    = "room_is_public"
  enc UserIsAdmin     = "user_is_admin"
  enc UserIsGuest     = "user_is_guest"
  enc UserIsRoomOwner = "user_is_room_owner"
  enc GlanceMatches   = "glance_matches"
  dec "room_is_public"     = Just RoomIsPublic
  dec "user_is_admin"      = Just UserIsAdmin
  dec "user_is_guest"      = Just UserIsGuest
  dec "user_is_room_owner" = Just UserIsRoomOwner
  dec "glance_matches"     = Just GlanceMatches
  dec _                    = Nothing

instance IsString Condition where
  fromString = AsText.fromString

instance ToJSON Condition where
  toJSON = AsText.toJSON

instance FromJSON Condition where
  parseJSON = AsText.parseJSON

data ConditionOperation = And | Or
  deriving (Show, Eq, Generic)

instance AsText ConditionOperation where
  enc And = "and"
  enc Or  = "or"
  dec "and" = Just And
  dec "AND" = Just And
  dec "or"  = Just Or
  dec "OR"  = Just Or
  dec _     = Nothing

instance IsString ConditionOperation where
  fromString = AsText.fromString

instance ToJSON ConditionOperation where
  toJSON = AsText.toJSON

instance FromJSON ConditionOperation where
  parseJSON = AsText.parseJSON

--TODO is arbitrary recursion depth actually supported?
data GlanceCondition
  = SimpleGlanceCondition -- TODO docs seem to imply this, but is it required?
    { glanceConditionCondition :: Condition }
  | SingleGlanceCondition
    { glanceConditionCondition :: Condition
    , glanceConditionInvert    :: Maybe Bool
    -- ^ A flag indicating whether to invert the boolean result of the condition.
    , glanceConditionParams    :: Maybe Object --TODO can we be more precise?
    -- ^ A map of key/value parameters for a built-in condition.
    }
  | CompoundGlanceCondition
    { glanceConditionType       :: ConditionOperation
    -- ^ Defines what logical operator is used to evaluate the list of conditions.
    , glanceConditionConditions :: [GlanceCondition] --TODO nonempty
    -- ^ The conditions to compose using the specific logical operator.
    }
    deriving (Eq, Show, Generic)

instance ToFromJSON GlanceCondition where
  variant = unwrappedUnaryRecords

--------------------------------------------------------------------------------
-- Glance
--------------------------------------------------------------------------------

data Glance = Glance
  { glanceIcon       :: Icon
  -- ^ Icon to display on the left side of the glance.
  , glanceKey        :: Key
  -- ^ Unique key (in the context of the integration) to identify this glance. Valid length range: 1 - 40.
  , glanceName       :: Name
  -- ^ The display name of the glance.
  , glanceQueryUrl   :: Maybe URL
  -- ^ The URL of the resource providing the glance content.
  , glanceTarget     :: Maybe GlanceTarget
  -- ^ Defines the behaviour when clicking on the glance.
  , glanceWeight     :: Maybe Integer
  -- ^ Determines the order in which glances appear.
  -- Glances are displayed top to bottom in order of ascending weight.
  -- Defaults to 100.
  , glanceConditions :: Maybe [GlanceCondition] --TODO nonempty
  } deriving (Eq, Generic, Show)

mkGlance :: Icon -> Key -> Name -> Glance
mkGlance i k n = Glance i k n Nothing Nothing Nothing Nothing

instance ToFromJSON Glance

makeLensesWith camelCaseFields ''Glance
