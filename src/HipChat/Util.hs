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
genericToJSON' = genericToJSON $ aesonOpts (Proxy :: Proxy a)

genericParseJSON' :: forall a. (Typeable a, Generic a, GFromJSON Zero (Rep a))
                  => Value -> Parser a
genericParseJSON' = genericParseJSON $ aesonOpts (Proxy :: Proxy a)

aesonOpts :: Typeable a => Proxy a -> Options
aesonOpts p = defaultOptions
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
  toJSON' = genericToJSON $ aesonOpts (Proxy :: Proxy a)

  parseJSON' :: Value -> Parser a
  default parseJSON' :: (Typeable a, Generic a, GFromJSON Zero (Rep a))
                     => Value -> Parser a
  parseJSON' = genericParseJSON $ aesonOpts (Proxy :: Proxy a)

instance {-# OVERLAPPABLE #-} ToFromJSON a => ToJSON a where
  toJSON = toJSON'

instance {-# OVERLAPPABLE #-} ToFromJSON a => FromJSON a where
  parseJSON = parseJSON'
