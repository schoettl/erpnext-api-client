{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Filters
  ( Filter (..)
  , FilterOperator (..)
  , FilterValue (..)
  , renderFilters
  ) where

import Data.Text (Text, intercalate)
import Data.Time.Calendar (Day)
import ERPNext.Client.Helper (urlEncode, quote, tshow)
import Prelude

-- TODO: refactor this? rename to filter and parameterize each term with fieldname and value?
data FilterOperator
  = Eq
  | NotEq
  | Greater
  | GreaterOrEq
  | Less
  | LessOrEq
  | Like
  | NotLike
  | In
  | NotIn
  | Between
  | Is
  deriving (Show, Eq)

data FilterValue
  = FilterText Text
  | FilterNumber Double
  | FilterBool Bool
  | FilterList [FilterValue]
  | FilterDay Day
  | FilterNull -- Used only with Is
  | FilterNotNull -- Used only with Is
  deriving (Show, Eq)

data Filter = Filter
  { filterField :: Text
  , filterOperator :: FilterOperator
  , filterValue :: FilterValue
  }
  deriving (Show, Eq)

renderFilterOperator :: FilterOperator -> Text
renderFilterOperator op =
  case op of
    Eq -> "="
    NotEq -> "!="
    Greater -> ">"
    GreaterOrEq -> ">="
    Less -> "<"
    LessOrEq -> "<="
    Like -> "like"
    NotLike -> "not like"
    In -> "in"
    NotIn -> "not in"
    Between -> "between"
    Is -> "is"

renderFilterValue :: FilterValue -> Text
renderFilterValue fv =
  case fv of
    FilterText t -> quote t
    FilterNumber n -> tshow n
    FilterBool b -> if b then "1" else "0"
    FilterList vs -> "[" <> intercalate ", " (map renderFilterValue vs) <> "]"
    FilterDay d -> quote (tshow d)
    FilterNull -> quote "not set"
    FilterNotNull -> quote "set"

renderFilter :: Filter -> Text
renderFilter f =
  "["
    <> quote (filterField f)
    <> ","
    <> quote (renderFilterOperator (filterOperator f))
    <> ","
    <> renderFilterValue (filterValue f)
    <> "]"

renderFilters :: Text -> [Filter] -> Text
renderFilters prefix filters =
  let
    encoded = map renderFilter filters
    str = "[" <> intercalate ", " encoded <> "]"
  in
    prefix <> "=" <> urlEncode str
