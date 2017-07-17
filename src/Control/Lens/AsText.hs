{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Lens.AsText
    ( AsText (..)
    , toUrlPiece
    , parseUrlPiece
    , toJSON
    , parseJSON
    , fromString
    ) where

import           Control.Lens     (Prism', matching, prism', re, review, (^.), preview,re)
import           Data.Aeson       (Value (String), withText)
import           Data.Aeson.Types (Parser)
import           Data.Bifunctor   (first)
import           Data.Proxy       (Proxy (Proxy))
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Typeable    (Typeable, typeRep)

class AsText a where
  parse :: Prism' Text a
  enc   :: a -> Text
  dec   :: Text -> Maybe a

  parse = prism' enc dec
  enc = review parse
  dec = preview parse
  {-# MINIMAL parse | enc, dec #-}

instance AsText Text where
  parse = id

toUrlPiece :: AsText a => a -> Text
toUrlPiece = review parse

parseUrlPiece :: forall a. (AsText a, Typeable a) => Text -> Either Text a
parseUrlPiece = first (T.pack . err (Proxy :: Proxy a)) . matching parse

toJSON :: AsText a => a -> Value
toJSON = String . (^. re parse)

parseJSON :: forall a. (AsText a, Typeable a) => Value -> Parser a
parseJSON = withText "text" (either (fail . err (Proxy :: Proxy a)) return . matching parse)

fromString :: forall a. (AsText a, Typeable a) => String -> a
fromString = either (error . err (Proxy :: Proxy a)) id . matching parse . T.pack

err :: Typeable a => Proxy a -> Text -> String
err p t = "Unexpected " ++ show (typeRep p) ++ ": " ++ T.unpack t
