{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Helper
  ( urlEncode
  , quote
  ) where

import Data.Text
import Data.Text qualified as T
import Network.URI (escapeURIString, isUnreserved)

urlEncode :: Text -> Text
urlEncode = pack . escapeURIString isUnreserved . unpack

sanitizeQuotes :: Text -> Text
sanitizeQuotes = T.filter (/= '"')

quote :: Text -> Text
quote t = "\"" <> sanitizeQuotes t <> "\""

