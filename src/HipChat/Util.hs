{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module HipChat.Util
  ( genericToJSON'
  , genericParseJSON'
  , ToFromJSON
  , opts
  , standardAesonOpts
  ) where

import           Data.Aeson.Types (FromJSON, GFromJSON, GToJSON, Options,
                                   Parser, ToJSON, Value, Zero, defaultOptions,
                                   fieldLabelModifier, genericParseJSON,
                                   genericToJSON, omitNothingFields, parseJSON,
                                   toJSON)
import           Data.Char        (toLower)
import           Data.Proxy       (Proxy (Proxy))
import           Data.Typeable    (Typeable, typeRep)

import           GHC.Generics

genericToJSON' :: forall a. (Typeable a, Generic a, GToJSON Zero (Rep a))
               => a -> Value
genericToJSON' = genericToJSON $ standardAesonOpts (Proxy :: Proxy a)

genericParseJSON' :: forall a. (Typeable a, Generic a, GFromJSON Zero (Rep a))
                  => Value -> Parser a
genericParseJSON' = genericParseJSON $ standardAesonOpts (Proxy :: Proxy a)

standardAesonOpts :: Typeable a => Proxy a -> Options
standardAesonOpts p = defaultOptions
  { fieldLabelModifier = uncapitalise . drop (numCharsInType p)
  , omitNothingFields  = True
  }

numCharsInType :: forall a. Typeable a => Proxy a -> Int
numCharsInType = length . show . typeRep

uncapitalise :: String -> String
uncapitalise []      = []
uncapitalise (h : t) = toLower h : t

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
  opts = standardAesonOpts


instance {-# OVERLAPPABLE #-} ToFromJSON a => ToJSON a where
  toJSON = toJSON'

instance {-# OVERLAPPABLE #-} ToFromJSON a => FromJSON a where
  parseJSON = parseJSON'
