{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.QueryStringParams where

import ERPNext.Client.Filters
import Data.Text hiding (map)


-- TODO: Placeholder
-- TODO: Maybe rename type to make it more abstract (not tied to the URL query string)?
-- TODO: Maybe change type or make opaque type to prevent invalid combinations?
data QueryStringParam = Asc Text | Desc Text | Fields [Text] | AndFilter [Filter] | OrFilter [Filter]

renderQueryStringParam :: QueryStringParam -> Text
renderQueryStringParam qsParam = 
  case qsParam of
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