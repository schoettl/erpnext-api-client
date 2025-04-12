{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Helper
  ( urlEncode
  , urlDecode
  , quote
  , tshow
  , Fieldname
  ) where

import Data.Text
import Data.Text qualified as T
import Network.URI (escapeURIString, isUnreserved, unEscapeString)

type Fieldname = Text

-- | Percent-encode string for use in a URL.
urlEncode :: Text -> Text
urlEncode = pack . escapeURIString isUnreserved . unpack

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

