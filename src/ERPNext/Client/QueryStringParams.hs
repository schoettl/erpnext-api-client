{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.QueryStringParams
  ( QueryStringParam (..)
  , renderQueryStringParams
  ) where

import ERPNext.Client.Filters
import ERPNext.Client.Helper (urlEncode, quote)
import Data.Text hiding (map)

-- https://docs.frappe.io/framework/user/en/api/rest

-- TODO: Maybe change type or make opaque type to prevent invalid combinations?
-- TODO: add variants for limit, offset, etc.?
data QueryStringParam
  = Asc Text
  | Desc Text
  | Fields [Text]
  | AndFilter [Filter]
  | OrFilter [Filter]

renderQueryStringParam :: QueryStringParam -> Text
renderQueryStringParam qsParam =
  case qsParam of
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

renderQueryStringParams :: [QueryStringParam] -> Text
renderQueryStringParams params = intercalate "&" (map renderQueryStringParam params)
