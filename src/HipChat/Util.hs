{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module HipChat.Util
  ( ToFromJSON
  , variant
  , camelCase
  , snakeCase
  , unwrappedUnaryRecords
  ) where

import           Data.Aeson.Types (FromJSON, GFromJSON, GToJSON, Options (..),
                                   Parser, SumEncoding (UntaggedValue), ToJSON,
                                   Value, Zero, allNullaryToStringTag,
                                   constructorTagModifier, fieldLabelModifier,
                                   genericParseJSON, genericToJSON,
                                   omitNothingFields, parseJSON, sumEncoding,
                                   toJSON, unwrapUnaryRecords)
import           Data.Char        (isLower, isUpper, toLower)
import           Data.List        (intercalate)
import           Data.Proxy       (Proxy (Proxy))
import           Data.Typeable    (Typeable, tyConName, typeRep, typeRepTyCon)

import           GHC.Generics

standardAesonOpts :: Typeable a => Proxy a -> Options
standardAesonOpts p = Options
  { fieldLabelModifier     = uncapitalise . drop (numCharsInType p)
  , constructorTagModifier = uncapitalise
  , allNullaryToStringTag  = True
  , omitNothingFields      = True
  , sumEncoding            = UntaggedValue
  , unwrapUnaryRecords     = False
  }

camelCase :: Options -> Options
camelCase = id

snakeCase :: Options -> Options
snakeCase o = o { fieldLabelModifier = toSnakeCase . fieldLabelModifier o}

unwrappedUnaryRecords :: Options -> Options
unwrappedUnaryRecords o = o { unwrapUnaryRecords = True }

class ToFromJSON a where
  toJSON' :: a -> Value
  default toJSON' :: (Typeable a, Generic a, GToJSON Zero (Rep a))
                  => a -> Value
  toJSON' = genericToJSON $ opts (Proxy :: Proxy a)

  parseJSON' :: Value -> Parser a
  default parseJSON' :: (Typeable a, Generic a, GFromJSON Zero (Rep a))
                     => Value -> Parser a
  parseJSON' = genericParseJSON $ opts (Proxy :: Proxy a)

  opts :: Typeable a => Proxy a -> Options
  opts = variant @ a . standardAesonOpts

  variant :: Options -> Options
  variant = camelCase

instance {-# OVERLAPPABLE #-} ToFromJSON a => ToJSON a where
  toJSON = toJSON'

instance {-# OVERLAPPABLE #-} ToFromJSON a => FromJSON a where
  parseJSON = parseJSON'

numCharsInType :: forall a. Typeable a => Proxy a -> Int
numCharsInType = length . tyConName . typeRepTyCon . typeRep

uncapitalise :: String -> String
uncapitalise []      = []
uncapitalise (h : t) = toLower h : t

toSnakeCase :: String -> String
toSnakeCase = map toLower . intercalate "_" . camelComponents

camelComponents :: String -> [String]
camelComponents = go [] ""
  where
    go :: [String] -> String -> String -> [String]
    go ws w (x:u:l:xs) | isUpper u && isLower l = go ((x:w):ws) [l, u] xs
    go ws w (l:u:xs)   | isUpper u && isLower l = go ((l:w):ws) [u] xs
    go ws w (x:xs)     = go ws (x:w) xs
    go ws w ""         = reverse (map reverse (w:ws))
