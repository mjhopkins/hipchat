{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HipChat.Util
  ( genericToJSON'
  , genericParseJSON'
  ) where

import           Data.Aeson.Types (GFromJSON, GToJSON, Options, Parser, Value,
                                   Zero, defaultOptions, fieldLabelModifier,
                                   genericParseJSON, genericToJSON,
                                   omitNothingFields)
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
