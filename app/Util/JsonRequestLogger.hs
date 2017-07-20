{-# LANGUAGE OverloadedStrings #-}

module Util.JsonRequestLogger where

import qualified Blaze.ByteString.Builder             as BB
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor                       (bimap)
import qualified Data.ByteString.Char8                as S8
import           Data.CaseInsensitive                 (original)
import           Data.IP
import           Data.Monoid                          ((<>))
import           Data.String.Conversions
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           Data.Time                            (NominalDiffTime)
import           Data.Word                            (Word32)
import           Network.HTTP.Types                   as H
import           Network.Socket                       (PortNumber,
                                                       SockAddr (..))
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           System.Log.FastLogger                (toLogStr)
import           Text.Printf                          (printf)

formatAsJSON :: OutputFormatterWithDetails
formatAsJSON date req _ _ _ reqBody response =
  toLogStr $
    date <> " " <> requestMethod req <> " " <> rawPathInfo req <>
     "\n" <>
     (cs . formatJson . cs . S8.concat $ reqBody) <>
     "\n" <>
     (cs . formatJson . decodeUtf8 . BB.toByteString $ response) <>
     "\n\n"

-- formatAsJSON :: OutputFormatterWithDetails
-- formatAsJSON date req status responseSize duration reqBody response =
--   toLogStr (showUnescaped . encodePretty $
--     object
--       [ "request"  .= requestToJSON duration req reqBody
--       , "response" .=
--       object
--         [ "status" .= statusCode status
--         , "size"   .= responseSize
--         , "body"   .=
--           if statusCode status >= 400
--             then Just . formatJson . decodeUtf8 . BB.toByteString $ response
--             else Nothing
--         ]
--       , "time"     .= decodeUtf8 date
--       ]) <> "\n"

formatJson :: Text -> Text
formatJson b = maybe b (cs . showUnescaped . encodePretty) (v b) where
  v :: Text -> Maybe Value
  v = decode . cs


word32ToHostAddress :: Word32 -> Text
word32ToHostAddress = T.intercalate "." . map (T.pack . show) . fromIPv4 . fromHostAddress

readAsDouble :: String -> Double
readAsDouble = read

requestToJSON :: NominalDiffTime -> Request -> [S8.ByteString] -> Value
requestToJSON duration req reqBody =
  object
    [ "method" .= decodeUtf8 (requestMethod req)
    , "path" .= decodeUtf8 (rawPathInfo req)
    , "queryString" .= map queryItemToJSON (queryString req)
    , "durationMs" .= (readAsDouble . printf "%.2f" . rationalToDouble $ toRational duration * 1000)
    , "size" .= requestBodyLengthToJSON (requestBodyLength req)
    , "body" .= formatJson (decodeUtf8 (S8.concat reqBody))
    , "remoteHost" .= sockToJSON (remoteHost req)
    , "httpVersion" .= httpVersionToJSON (httpVersion req)
    , "headers" .= requestHeadersToJSON (requestHeaders req)
    ]
  where
    rationalToDouble :: Rational -> Double
    rationalToDouble = fromRational

sockToJSON :: SockAddr -> Value
sockToJSON (SockAddrInet pn ha) = object
    [ "port" .= portToJSON pn
    , "hostAddress" .= word32ToHostAddress ha
    ]
sockToJSON (SockAddrInet6 pn _ ha _) = object
    [ "port" .= portToJSON pn
    , "hostAddress" .= ha
    ]
sockToJSON (SockAddrUnix sock) = object [ "unix" .= sock ]
sockToJSON (SockAddrCan i)     = object [ "can" .= i ]

queryItemToJSON :: QueryItem -> Value
queryItemToJSON (name, mValue) = toJSON (decodeUtf8 name, fmap decodeUtf8 mValue)

requestHeadersToJSON :: RequestHeaders -> Value
requestHeadersToJSON = toJSON . map hToJ where
  -- Redact cookies
  hToJ ("Cookie", _) = toJSON ("Cookie" :: Text, "-RDCT-" :: Text)
  hToJ hd            = headerToJSON hd

headerToJSON :: Header -> Value
headerToJSON (headerName, header) = toJSON (decodeUtf8 . original $ headerName, decodeUtf8 header)

portToJSON :: PortNumber -> Value
portToJSON = toJSON . toInteger

httpVersionToJSON :: HttpVersion -> Value
httpVersionToJSON (HttpVersion major minor) = String $ T.pack (show major) <> "." <> T.pack (show minor)

requestBodyLengthToJSON :: RequestBodyLength -> Value
requestBodyLengthToJSON ChunkedBody     = String "Unknown"
requestBodyLengthToJSON (KnownLength l) = toJSON l




prettyPrint :: ToJSON a => a -> IO ()
prettyPrint = putStrLn . showUnescaped . encodePretty

showUnescaped :: Show a => a -> String
showUnescaped = replace [("\\n", "\n"), ("\\\"", "\"")] . show

replace :: [(String, String)] -> String -> String
replace assocs s = T.unpack $ foldr replace' (T.pack s) assocs'
  where
        assocs' :: [(Text, Text)]
        assocs'   = map (bimap T.pack T.pack) assocs
        replace' :: (Text, Text) -> Text -> Text
        replace' (a,b) = T.replace a b

printEncoding :: ToJSON a => a -> IO ()
printEncoding = putStrLn . showUnescaped . encodePretty
