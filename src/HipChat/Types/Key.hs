{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Key where

import           Text.Regex.PCRE

--------------------------------------------------------------------------------
-- |
-- Module: HipChat.Key
--
-- Length-restricted key type (1-40 chars); should be unique (within some context).
--------------------------------------------------------------------------------

import           Control.Lens
import           Control.Lens.AsText       (AsText, parse)
import qualified Control.Lens.AsText       as AsText
import           Data.Aeson
import           Data.Monoid
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import Data.Proxy

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show)

keyRegex :: String
keyRegex = "^[a-zA-Z0-9_\\-\\.]{1,40}$"

instance AsText Key where
  enc = unKey
  dec t = if T.unpack t =~ keyRegex
          then Just . Key $ t
          else Nothing
  -- dec t = if l >= 1 && l <= 40
  --         then Just $ Key t
  --         else Nothing
  --         where l = T.length t

-- key :: QuasiQuoter
-- key = QuasiQuoter
--   { quoteExp = \s ->
--       case parse s of
--         Left err -> fail err
--         Right v  -> [| case $(varE )|]
--   , quotePat  = unsupported "pattern"
--   , quoteType = unsupported "type"
--   , quoteDec  = unsupported "declaration"
--   }
--   where
--     unsupported cxt = fail $
--       "This quasiquoter can't be used in a " ++ cxt ++ " context"

keyQQ :: QuasiQuoter
keyQQ = AsText.qq (Proxy :: Proxy Key)

-- instance IsString Key where
  -- fromString = AsText.fromString
  -- fromString s = case s ^? parse
  -- fromString s =
    -- if s =~ ("^[a-zA-Z0-9_\\-\\.]+$" :: String)
    -- then Key . T.pack $ s
    -- else error "Invalid key"

instance ToJSON Key where
  toJSON (Key x) = toJSON x

instance FromJSON Key where
  parseJSON x = do
    rawKey <- parseJSON x
    case rawKey ^? parse of
      Nothing -> fail $ "Invalid key length, expected 1..40, got: " <> show (T.length rawKey)
      Just k  -> return k
