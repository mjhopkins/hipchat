module HipChat.Types.URL
  ( URL
  , (/)
  , parseURL
  ) where

import           Blaze.ByteString.Builder  (toLazyByteString)

import           Data.Aeson                (FromJSON, ToJSON, parseJSON, toJSON,
                                            withText)
import qualified Data.ByteString.Lazy.UTF8 as LB
import           Data.List                 (isSuffixOf)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.String               (IsString, fromString)
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Network.HTTP.Types        (encodePathSegments)
import           Network.URI               (URI)
import qualified Network.URI               as URI

import           Prelude                   hiding ((/))

-- | Wraps an absolute URI
newtype URL = URL URI
  deriving (Eq)

instance Show URL where
  show (URL u) = "\"" ++ show u ++ "\""

instance IsString URL where
  fromString s =
    fromMaybe (error $ "Not a valid URL: " <> s) (parseURL s)

instance ToJSON URL where
  toJSON (URL u) = toJSON $ show u

instance FromJSON URL where
  parseJSON = withText "String" $ \t ->
    maybe (fail $ "Not a valid URL: " ++ T.unpack t) return . parseURL . T.unpack $ t

parseURL :: String -> Maybe URL
parseURL = fmap URL . URI.parseAbsoluteURI

appendPath :: URL -> [Text] -> URL
appendPath (URL uri) xs = URL uri' where
  uri' = uri { URI.uriPath = URI.uriPath uri <> dropSlash (relPath xs) }
  dropSlash =
    if "/" `isSuffixOf` URI.uriPath uri
    then tail
    else id

(/) :: URL -> Text -> URL
uri / t = appendPath uri [t]

relPath :: [Text] -> String
relPath = LB.toString . toLazyByteString . encodePathSegments
