{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Helper
  ( urlEncode
  , urlDecode
  , quote
  , tshow
  , Fieldname
  ) where

import Data.Text
import Network.URI (escapeURIString, isUnreserved, unEscapeString)

-- | Type for field names of DocTypes.
type Fieldname = Text

-- | Percent-encode string for use in a URL.
urlEncode :: Text -> Text
urlEncode = pack . escapeURIString isUnreserved . unpack

-- | Opposite of 'urlEncode'.
urlDecode :: Text -> Text
urlDecode = pack . unEscapeString . unpack

sanitizeQuotes :: Text -> Text
sanitizeQuotes = replace "\"" "\\\""

-- | Double-quote string after backslash-escaping existing double quotes.
quote :: Text -> Text
quote t = "\"" <> sanitizeQuotes t <> "\""

-- | 'show' but return 'Text'.
tshow :: Show a => a -> Text
tshow = pack . show

