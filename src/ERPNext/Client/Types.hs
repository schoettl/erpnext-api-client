
module ERPNext.Client.Types
  ( DocStatus (..)
  , EBool (..)
  , UploadedFile (..)
  , toDocStatus
  , fromDocStatus
  , toBool
  , fromBool
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Scientific (toBoundedInteger)
import GHC.Generics (Generic)

-- | ERPNext Bool type for use in your DocType record definition.
--
-- You cannot use 'Bool' there because ERPNext encodes boolean values as integer.
-- Use this 'EBool' if you don't want to use 'Int'.
newtype EBool = EBool { isTrue :: Bool }

instance FromJSON EBool where
  parseJSON (Bool b) = return $ EBool b
  parseJSON (Number n) =
    case toBoundedInteger n of
      Just i -> return $ EBool $ toBool i
      Nothing -> fail $ "expected integer but got non-integer number: " ++ show n
  parseJSON v = fail $ "expected Bool or Number but got " ++ show v

-- | DocType status type for use in your DocType record definition.
--
-- https://docs.frappe.io/framework/user/en/basics/doctypes/frameworktatus
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

data UploadedFile = UploadedFile
  { name       :: Text
  , file_url   :: Text
  , file_name  :: Text
  , is_private :: Int
  } deriving (Show, Generic)

instance FromJSON UploadedFile

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
