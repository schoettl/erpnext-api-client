{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.QueryStringParams
  ( QueryStringParam (..)
  , renderQueryStringParams
  ) where

import ERPNext.Client.Filters
import ERPNext.Client.Helper
import Data.Text hiding (map)

-- https://docs.frappe.io/framework/user/en/api/rest

-- TODO: Maybe change type or make opaque type to prevent invalid combinations?
-- TODO: add variants for limit, offset, etc.?
data QueryStringParam
  = Debug Bool -- ^ If 'True', makes API returning query analysis info instead of data
  | AsDict Bool -- ^ If 'False', makes API returning the data records as mixed-type arrays which cannot be parsed by this library (default: 'True')
  | LimitStart Int -- ^ Page offset (starts at 0)
  | LimitPageLength Int -- ^ Page size
  | Asc Fieldname -- ^ Ascending order by given field
  | Desc Fieldname -- ^ Descending order by given field
  | Fields [Fieldname] -- ^ Select fields
  | AndFilter [Filter] -- ^ Filter terms combined with logical AND
  | OrFilter [Filter] -- ^ Filter terms combined with logical OR

renderQueryStringParam :: QueryStringParam -> Text
renderQueryStringParam qsParam =
  case qsParam of
    Debug b -> "debug=" <> tshow b
    AsDict b -> "as_dict=" <> tshow b
    LimitStart offset -> "limit_start=" <> tshow (max 0 offset)
    LimitPageLength n -> "limit=" <> tshow (max 0 n) -- it was named limit_page_length up to v13

    Asc field ->
      "order_by=" <> renderOrderBy field "asc"

    Desc field ->
      "order_by=" <> renderOrderBy field "desc"

    Fields fields ->
      "fields=" <> renderFields fields

    AndFilter filters ->
      "filters=" <> renderFilters filters

    OrFilter filters ->
      "or_filters=" <> renderFilters filters

renderFields :: [Fieldname] -> Text
renderFields fields =
  urlEncode ("[" <> intercalate "," (map quote fields) <> "]")

renderOrderBy :: Fieldname -> Text -> Text
renderOrderBy field order =
  urlEncode (field <> " " <> order)

-- | Render the query string for the URL.
--
-- >>> renderQueryStringParams []
-- ""
--
-- >>> renderQueryStringParams [LimitStart 20, LimitPageLength 10]
-- "limit_start=20&limit=10"
--
-- >>> renderQueryStringParams [Fields [pack "id", pack "name"], Asc (pack "idx")]
-- "fields=[\"id\",\"name\"]&order_by=idx%20asc"
--
renderQueryStringParams :: [QueryStringParam] -> Text
renderQueryStringParams params = intercalate "&" (map renderQueryStringParam params)
