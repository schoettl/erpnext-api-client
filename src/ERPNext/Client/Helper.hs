{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Helper
  ( urlEncode
  , urlDecode
  , quote
  , tshow
  , Fieldname
  ) where

import Data.Text (Text, pack, unpack, replace)
import Network.URI (escapeURIString, isUnreserved, unEscapeString)

-- | Type for field names of DocTypes.
type Fieldname = Text

-- | Percent-encode string for use in a URL.
--
-- >>> urlEncode $ pack "[ ]"
-- "%5B%20%5D"
urlEncode :: Text -> Text
urlEncode = pack . escapeURIString isUnreserved . unpack

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

