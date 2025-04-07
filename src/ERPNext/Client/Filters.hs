{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Filters
  ( Filter (..)
  , Fieldname
  , FilterValue (..)
  , renderFilters
  ) where

import Data.Text (Text, intercalate)
import Data.Time.Calendar (Day)
import ERPNext.Client.Helper (urlEncode, quote, tshow)

type Fieldname = Text

data Filter
  = Eq Fieldname FilterValue
  | NotEq Fieldname FilterValue
  | Greater Fieldname FilterValue
  | GreaterOrEq Fieldname FilterValue
  | Less Fieldname FilterValue
  | LessOrEq Fieldname FilterValue
  | Like Fieldname FilterValue
  | NotLike Fieldname FilterValue
  | In Fieldname [FilterValue]
  | NotIn Fieldname [FilterValue]
  | Between Fieldname FilterValue
  | IsNull Fieldname
  | IsNotNull Fieldname
  deriving (Show, Eq)

data FilterValue
  = FilterText Text
  | FilterNumber Double
  | FilterBool Bool
  | FilterList [FilterValue]
  | FilterDay Day
  deriving (Show, Eq)

renderFilter :: Filter -> Text
renderFilter f =
  case f of
    Eq field val         -> render field "=" val
    NotEq field val      -> render field "!=" val
    Greater field val    -> render field ">" val
    GreaterOrEq field val-> render field ">=" val
    Less field val       -> render field "<" val
    LessOrEq field val   -> render field "<=" val
    Like field val       -> render field "like" val
    NotLike field val    -> render field "not like" val
    In field vals        -> render field "in" (FilterList vals)
    NotIn field vals     -> render field "not in" (FilterList vals)
    Between field val    -> render field "between" val
    IsNull field         -> "[" <> quote field <> "," <> quote "is" <> "," <> quote "not set" <> "]"
    IsNotNull field         -> "[" <> quote field <> "," <> quote "is" <> "," <> quote "set" <> "]"
  where
    render field op val =
      "["
        <> quote field <> ","
        <> quote op <> ","
        <> renderFilterValue val
        <> "]"


renderFilterValue :: FilterValue -> Text
renderFilterValue fv =
  case fv of
    FilterText t -> quote t
    FilterNumber n -> tshow n
    FilterBool b -> if b then "1" else "0"
    FilterList vs -> "[" <> intercalate ", " (map renderFilterValue vs) <> "]"
    FilterDay d -> quote (tshow d)

-- | Render the filter terms for the URL query string.
renderFilters :: Text -> [Filter] -> Text
renderFilters prefix filters =
  let
    encoded = map renderFilter filters
    str = "[" <> intercalate ", " encoded <> "]"
  in
    prefix <> "=" <> urlEncode str
