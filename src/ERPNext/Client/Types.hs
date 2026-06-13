
module ERPNext.Client.Types
  ( DocStatus (..)
  , EBool (..)
  , toDocStatus
  , fromDocStatus
  , toBool
  , fromBool
  ) where

import Data.Aeson

-- | ERPNext Bool type.
newtype EBool = EBool { isTrue :: Bool }

instance FromJSON EBool where
  parseJSON (Bool b) = return $ EBool b
  parseJSON (Number i) = return $ EBool $ toBool $ round i -- TODO: fail if not integer
  parseJSON _ = fail "expected integer (decoded as bool) but got something else" -- TODO

-- | https://docs.frappe.io/framework/user/en/basics/doctypes/frameworktatus
data DocStatus
  = Draft
  | Submitted
  | Cancelled
  | Other Int

instance FromJSON DocStatus where
  parseJSON (Number i) = return $ toDocStatus $ round i -- TODO: if not integer throw error
  parseJSON _ = fail "expected integer but got something else"

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
