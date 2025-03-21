{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.QueryStringParams
  ( QueryStringParam (..)
  , renderQueryStringParams
  ) where

import ERPNext.Client.Filters
import ERPNext.Client.Helper (urlEncode, quote, tshow)
import Data.Text hiding (map)

-- https://docs.frappe.io/framework/user/en/api/rest

-- TODO: Maybe change type or make opaque type to prevent invalid combinations?
-- TODO: add variants for limit, offset, etc.?
data QueryStringParam
  = Debug Bool -- ^ If 'True', makes API returning query analysis info instead of data
  | AsDict Bool -- ^ If 'False', makes API returning the data records as mixed-type arrays which cannot be parsed by this library (default: 'True')
  | LimitStart Int -- ^ Page offset
  | LimitPageLength Int -- ^ Page size
  | Asc Text -- ^ Ascending order by given field
  | Desc Text -- ^ Descending order by given field
  | Fields [Text] -- ^ Select fields
  | AndFilter [Filter] -- ^ Filter terms combined with logical AND
  | OrFilter [Filter] -- ^ Filter terms combined with logical OR

renderQueryStringParam :: QueryStringParam -> Text
renderQueryStringParam qsParam =
  case qsParam of
    Debug b -> "debug=" <> tshow b
    AsDict b -> "as_dict=" <> tshow b
    LimitStart offset -> "limit_start=" <> tshow offset
    LimitPageLength n -> "limit=" <> tshow n -- it was limit_page_length up to v13

    Asc field ->
      renderOrderBy field "asc"

    Desc field ->
      renderOrderBy field "desc"

    Fields fields ->
      renderFields fields

    AndFilter filters ->
      renderFilters "filters" filters

    OrFilter filters ->
      renderFilters "or_filters" filters

renderFields :: [Text] -> Text
renderFields fields =
  "fields=" <> urlEncode ("[" <> intercalate "," (map quote fields) <> "]")

renderOrderBy :: Text -> Text -> Text
renderOrderBy field order =
  "order_by=" <> urlEncode (field <> " " <> order)

-- | Render the query string for the URL.
renderQueryStringParams :: [QueryStringParam] -> Text
renderQueryStringParams params = intercalate "&" (map renderQueryStringParam params)
