{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.QueryStringParams
  ( QueryStringParam (..)
  , renderQueryStringParams
  ) where

import ERPNext.Client.Filters
import Data.Text hiding (map)

-- TODO: Maybe change type or make opaque type to prevent invalid combinations?
data QueryStringParam
  = Asc Text
  | Desc Text
  | Fields [Text]
  | AndFilter [Filter]
  | OrFilter [Filter]

renderQueryStringParam :: QueryStringParam -> Text
renderQueryStringParam qsParam =
  case qsParam of
    -- TODO: implement
    Asc _ ->
      ""
    Desc _ ->
      ""
    Fields _ ->
      ""
    AndFilter filters ->
      renderFilters "filters" filters

    OrFilter filters ->
      renderFilters "or_filters" filters


renderQueryStringParams :: [QueryStringParam] -> Text
renderQueryStringParams params = intercalate "&" (map renderQueryStringParam params)
