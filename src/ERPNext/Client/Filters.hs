{-# LANGUAGE OverloadedStrings #-}

module ERPNext.Client.Filters
  ( Filter (..)
  , Fieldname
  , FilterValue (..)
  , renderFilters
  ) where

import Data.Text (Text, intercalate)
import Data.Time.Calendar (Day)
import ERPNext.Client.Helper

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
  | Between Fieldname FilterValue FilterValue
  | IsNull Fieldname
  | IsNotNull Fieldname
  deriving (Show, Eq)

data FilterValue
  = FilterText Text
  | FilterNumber Double
  | FilterBool Bool
  | FilterDay Day
  deriving (Show, Eq)

renderFilter :: Filter -> Text
renderFilter f =
  case f of
    Eq field val         -> renderWithValue field "=" val
    NotEq field val      -> renderWithValue field "!=" val
    Greater field val    -> renderWithValue field ">" val
    GreaterOrEq field val-> renderWithValue field ">=" val
    Less field val       -> renderWithValue field "<" val
    LessOrEq field val   -> renderWithValue field "<=" val
    Like field val       -> renderWithValue field "like" val
    NotLike field val    -> renderWithValue field "not like" val
    In field vals        -> renderWithArray field "in" vals
    NotIn field vals     -> renderWithArray field "not in" vals
    Between field val1 val2    -> renderWithArray field "between" [val1, val2]
    IsNull field         -> renderWithText field "is" "not set"
    IsNotNull field      -> renderWithText field "is" "set"

renderWithValue :: Fieldname -> Text -> FilterValue -> Text
renderWithValue field op val =
  renderTextArray [quote field, quote op, renderFilterValue val]

renderWithText :: Fieldname -> Text -> Text -> Text
renderWithText field op txt =
  renderTextArray [quote field, quote op, quote txt]

renderWithArray :: Fieldname -> Text -> [FilterValue] -> Text
renderWithArray field op vals =
  renderTextArray [quote field, quote op, renderTextArray (map renderFilterValue vals)]

renderTextArray :: [Text] -> Text
renderTextArray xs =
  "[" <> intercalate "," xs <> "]"

renderFilterValue :: FilterValue -> Text
renderFilterValue fv =
  case fv of
    FilterText t -> quote t
    FilterNumber n -> tshow n
    FilterBool b -> if b then "1" else "0"
    FilterDay d -> quote (tshow d)

-- | Render the filter terms for the URL query string.
renderFilters :: [Filter] -> Text
renderFilters filters =
  let
    encoded = map renderFilter filters
    str = "[" <> intercalate "," encoded <> "]"
  in
    urlEncode str
