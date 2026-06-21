
module ERPNext.Client.Types
  ( DocStatus (..)
  , EBool (..)
  , toDocStatus
  , fromDocStatus
  , toBool
  , fromBool
  ) where

import Data.Aeson
import Data.Scientific (toBoundedInteger)

-- | ERPNext Bool type.
newtype EBool = EBool { isTrue :: Bool }

instance FromJSON EBool where
  parseJSON (Bool b) = return $ EBool b
  parseJSON (Number n) =
    case toBoundedInteger n of
      Just i -> return $ EBool $ toBool i
      Nothing -> fail $ "expected integer but got non-integer number: " ++ show n
  parseJSON v = fail $ "expected Bool or Number but got " ++ show v

-- | https://docs.frappe.io/framework/user/en/basics/doctypes/frameworktatus
data DocStatus
  = Draft
  | Submitted
  | Cancelled
  | Other Int

instance FromJSON DocStatus where
  parseJSON (Number n) =
    case toBoundedInteger n of
      Just i -> return $ toDocStatus i
      Nothing -> fail $ "expected integer but got non-integer number: " ++ show n
  parseJSON v = fail $ "expected Number but got " ++ show v

fromDocStatus :: DocStatus -> Int
fromDocStatus status = case status of
  Draft -> 0
  Submitted -> 1
  Cancelled -> 2
  Other n -> n

toDocStatus :: Int -> DocStatus
toDocStatus status = case status of
  0 -> Draft
  1 -> Submitted
  2 -> Cancelled
  n -> Other n

-- | Decode from ERPNext bool representation.
toBool :: Int -> Bool
toBool i = i /= 0

-- | Encode to ERPNext bool representation.
fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1
