{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Helper
  ( urlEncode
  , urlEncodeQueryString
  , urlDecode
  , showJsonPretty
  , quote
  , tshow
  , isNotFound
  , isForbidden
  , toBool
  , fromBool
  , Fieldname
  ) where

import Data.Text (Text, pack, unpack, replace)
import Network.URI (escapeURIString, isUnreserved, unEscapeString)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Network.HTTP.Client
import Network.HTTP.Types (Status(statusCode))

-- | Type for field names of DocTypes.
type Fieldname = Text

-- | Percent-encode string for use in a URL including the characters "&=".
--
-- >>> urlEncode $ pack "[&]"
-- "%5B%26%5D"
urlEncode :: Text -> Text
urlEncode = pack . escapeURIString isUnreserved . unpack

-- | Percent-encode string for use in a URL but not the characters "&=".
--
-- This retains "&" to separate key-value pairs and "=" to separate keys from values.
--
-- >>> urlEncodeQueryString $ pack "fields=[\"project_name\"]&page_limit_length=10"
-- "fields=%5B%22project_name%22%5D&page_limit_length=10"
urlEncodeQueryString :: Text -> Text
urlEncodeQueryString = pack . escapeURIString isUnresveredInQueryString . unpack
  where
    isUnresveredInQueryString c = isUnreserved c || c `elem` ("&=" :: String)

-- | Opposite of 'urlEncode'.
--
-- >>> urlDecode $ pack "%20"
-- " "
urlDecode :: Text -> Text
urlDecode = pack . unEscapeString . unpack

-- | Show JSON in pretty-print format.
showJsonPretty :: ToJSON a => a -> String
showJsonPretty = TL.unpack . decodeUtf8 . encodePretty  -- TODO: better use decodeUtf8Lenient?

sanitizeQuotes :: Text -> Text
sanitizeQuotes = replace "\"" "\\\""

-- | Double-quote string after backslash-escaping existing double quotes.
--
-- >>> quote $ pack "text with \""
-- "\"text with \\\"\""
quote :: Text -> Text
quote t = "\"" <> sanitizeQuotes t <> "\""

-- | 'show' but return 'Text'. text-2.1.2 adds a new function 'show'
-- that could make this one redundant.
tshow :: Show a => a -> Text
tshow = pack . show

-- | Decode from ERPNext bool representation.
toBool :: Int -> Bool
toBool i = i /= 0

-- | Encode to ERPNext bool representation.
fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

-- | Test if response is HTTP 404 Not Found.
isNotFound :: Response a -> Bool
isNotFound = (==404) . statusCode . responseStatus

-- | Test if response is HTTP 403 Forbidden.
isForbidden :: Response a -> Bool
isForbidden = (==403) . statusCode . responseStatus
