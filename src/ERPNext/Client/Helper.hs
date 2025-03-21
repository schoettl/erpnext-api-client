{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Helper
  ( urlEncode
  , quote
  , tshow
  ) where

import Data.Text
import Data.Text qualified as T
import Network.URI (escapeURIString, isUnreserved)

-- | Percent-encode string for use in a URL.
urlEncode :: Text -> Text
urlEncode = pack . escapeURIString isUnreserved . unpack

sanitizeQuotes :: Text -> Text
sanitizeQuotes = T.filter (/= '"')

-- TODO: is it a good idea to just drop "?
-- | Double-quote string after dropping existing double quotes.
quote :: Text -> Text
quote t = "\"" <> sanitizeQuotes t <> "\""

-- | 'show' but return 'Text'.
tshow :: Show a => a -> Text
tshow = pack . show

