{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Helper
  ( urlEncode
  , urlEncodePieces
  , urlDecode
  , quote
  , tshow
  , Fieldname
  ) where

import Data.Text (Text, pack, unpack, replace)
import Network.URI (escapeURIString, isUnreserved, unEscapeString)

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
-- >>> urlEncodePieces $ pack "fields=[\"project_name\"]&page_limit_length=10"
-- "fields=%5B%22project_name%22%5D&page_limit_length=10"
urlEncodePieces :: Text -> Text
urlEncodePieces = pack . escapeURIString isUnresveredInPiece . unpack
  where
    isUnresveredInPiece c = isUnreserved c || c `elem` ("&=" :: String)

-- | Opposite of 'urlEncode'.
--
-- >>> urlDecode $ pack "%20"
-- " "
urlDecode :: Text -> Text
urlDecode = pack . unEscapeString . unpack

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
