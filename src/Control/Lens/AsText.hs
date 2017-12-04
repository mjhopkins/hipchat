{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Lens.AsText
    ( AsText (..)
    , toUrlPiece
    , parseUrlPiece
    , decodeEither
    , decodeEitherS
    , toJSON
    , parseJSON
    , fromString
    , qq
    ) where

import           Control.Lens              (Prism', matching, preview, prism',
                                            re, re, review, (^.))
import           Data.Aeson                (Value (String), withText)
import           Data.Aeson.Types          (Parser)
import           Data.Bifunctor            (first)
import           Data.Proxy                (Proxy (Proxy))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable, typeRep)

import  Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote

class AsText a where
  parse :: Prism' Text a
  enc   :: a -> Text
  dec   :: Text -> Maybe a

  parse = prism' enc dec
  enc = review parse
  dec = preview parse
  {-# MINIMAL parse | enc, dec #-}

decodeEither :: (AsText a, Typeable a) => Text -> Either Text a
decodeEither = parseUrlPiece

decodeEitherS :: forall a. (AsText a, Typeable a) => String -> Either String a
decodeEitherS = first (err (Proxy :: Proxy a)) . matching parse . T.pack

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
err p t = "Bad " ++ show (typeRep p) ++ ": \"" ++ T.unpack t ++ "\""

qq :: forall a. (AsText a, Typeable a) => Proxy a -> QuasiQuoter
qq _ = QuasiQuoter {..}
  where
  quoteExp :: String -> Q Exp
  quoteExp str =
    case decodeEitherS str of
      Right (_ :: a)  -> [|case decodeEitherS str of { Right x -> x; Left _ -> error "Unexpected" } |]
      Left err -> fail err

  quotePat  = unsupported "pattern"
  quoteType = unsupported "type"
  quoteDec  = unsupported "declaration"

  unsupported context = fail $
    "Unsupported operation: this QuasiQuoter can not be used in a " ++ context ++ " context."

qq' :: forall a. (AsText a, Typeable a) => Proxy a -> QuasiQuoter
qq' _ = QuasiQuoter {..}
  where
  quoteExp :: String -> Q Exp
  quoteExp str =
    case decodeEitherS str of
      Right (_ :: a)  -> [|case decodeEitherS str of { Right x -> x; Left _ -> error "Unexpected" } |]
      Left err -> fail err

  quotePat  = unsupported "pattern"
  quoteType = unsupported "type"
  quoteDec  = unsupported "declaration"

  unsupported context = fail $
    "Unsupported operation: this QuasiQuoter can not be used in a " ++ context ++ " context."

qqLiteral :: (String -> Either String a) -> Name -> QuasiQuoter
qqLiteral parse parseFn = QuasiQuoter {..}
  where
  quoteExp str =
    case parse str of
      Right _  -> [| case $(varE parseFn) str of { Right x -> x } |]
      Left err -> fail err

  quotePat  = unsupported "pattern"
  quoteType = unsupported "type"
  quoteDec  = unsupported "declaration"

  unsupported context = fail $
    "Unsupported operation: this QuasiQuoter can not be used in a " ++ context ++ " context."
