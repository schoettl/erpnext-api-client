{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Filters where

import Data.Text (Text, filter, intercalate, pack, unpack)
import Data.Time.Calendar (Day)
import Network.URI (escapeURIString, isUnreserved)
import Prelude hiding (filter)

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

data Filters
  = Single Filter
  | And [Filters]
  | Or [Filters]
  deriving (Show, Eq)

sanitizeQuotes :: Text -> Text
sanitizeQuotes = filter (/= '"')

quote :: Text -> Text
quote t = "\"" <> sanitizeQuotes t <> "\""

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
    FilterNumber n -> pack (show n)
    FilterBool b -> if b then "1" else "0"
    FilterList vs -> "[" <> intercalate ", " (map renderFilterValue vs) <> "]"
    FilterDay d -> quote (pack (show d))
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

flattenFilters :: Filters -> [Filter]
flattenFilters expr =
  case expr of
    Single f -> [f]
    And fs -> concatMap flattenFilters fs
    Or fs -> concatMap flattenFilters fs

urlEncodeValue :: Text -> Text
urlEncodeValue = pack . escapeURIString isUnreserved . unpack

renderFilters :: Filters -> Text
renderFilters filters =
  let
    prefix =
      case filters of
        Or _ -> "or_filters"
        _ -> "filters"
    filtersList = flattenFilters filters
    encoded = map renderFilter filtersList
    str = "[" <> intercalate ", " encoded <> "]"
  in
    prefix <> "=" <> urlEncodeValue str
