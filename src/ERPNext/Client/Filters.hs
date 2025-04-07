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
    In field vals        -> renderWithText field "in" (renderFilterValueList vals)
    NotIn field vals     -> renderWithText field "not in" (renderFilterValueList vals)
    Between field val1 val2    -> renderWithText field "between" (renderFilterValueList [val1, val2])
    IsNull field         -> renderWithText field "is" "not set"
    IsNotNull field      -> renderWithText field "is" "set"

renderWithValue :: Fieldname -> Text -> FilterValue -> Text
renderWithValue field op val =
  "[" <> quote (field) <> "," <> quote op <> "," <> renderFilterValue val <> "]"

renderWithText :: Fieldname -> Text -> Text -> Text
renderWithText field op txt =
  "[" <> quote (field) <> "," <> quote op <> "," <> quote txt <> "]"


renderFilterValue :: FilterValue -> Text
renderFilterValue fv =
  case fv of
    FilterText t -> quote t
    FilterNumber n -> tshow n
    FilterBool b -> if b then "1" else "0"
    FilterDay d -> quote (tshow d)

renderFilterValueList :: [FilterValue] -> Text
renderFilterValueList fvl = "[" <> intercalate ", " (map renderFilterValue fvl) <> "]"

-- | Render the filter terms for the URL query string.
renderFilters :: Text -> [Filter] -> Text
renderFilters prefix filters =
  let
    encoded = map renderFilter filters
    str = "[" <> intercalate ", " encoded <> "]"
  in
    prefix <> "=" <> urlEncode str
