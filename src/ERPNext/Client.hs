{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Generic API client library for ERPNext

This is a Haskell API client for ERPNext. It aims to be a light-weight
library based on http-client and user-provided record types.

API documentation:

https://docs.frappe.io/framework/user/en/api/rest
-}

module ERPNext.Client
  ( getDocList
  , getDoc
  , postDoc
  , putDoc
  , deleteDoc
  , mkSecret
  , mkConfig
  , IsDocType (..)
  , Config
  , Secret
  , QueryStringParam (..)
  , ApiResponse (..)
  , Fieldname
  , getResponse
  , andThen
  , andThenWith
  , getDocListAllFields
  , getAllFieldnames
  , systemFieldnames
  , showJsonResponsePretty
  ) where

import Network.HTTP.Client (Response (..), Manager)
import Data.Text hiding (map, filter, null)
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import ERPNext.Client.Filter (Fieldname)
import ERPNext.Client.QueryStringParam
import ERPNext.Client.Simple qualified as Simple
import ERPNext.Client.Simple (ApiResponse (..), Config, Secret, mkSecret, mkConfig, showJsonResponsePretty)

-- | Type class for types which represent an ERPNext DocType.
-- Each DocType has a unique name but there can still be multiple
-- „views“ (i.e. records types) for one DocType.
class IsDocType a where
  -- | The DocType name, e.g. „Sales Order“.
  docTypeName :: Text
  -- | The document identifier - called @name@ in ERPNext.
  docName :: a -> Text

{-|
  Get a list of all documents of a given DocType.
  The 'QueryStringParam's can select fields, filter, order, enable
  paging, and more.

  Warning: The resulting list is limited to 20 items by default
  (@limit_page_length=20@, see API documentation <https://docs.frappe.io/framework/user/en/api/rest#listing-documents>).
  Use 'ERPNext.Client.QueryStringParam.LimitPageLength' to set a different limit.
-}
getDocList :: forall a. (IsDocType a, FromJSON a)
               => Manager -> Config -> [QueryStringParam] -> IO (ApiResponse [a])
getDocList manager config qsParams = do
  let queryString = if null qsParams then Nothing else Just (renderQueryStringParams qsParams)
  response <- Simple.getDocList manager config (docTypeName @a) queryString
  return $ parseListResponse response
  where
    parseListResponse (Err resp err) = Err resp err
    parseListResponse (Ok resp val values) =
      case traverse fromJSON values of
        Success parsedList -> Ok resp val parsedList
        Error err -> Err resp (Just (val, pack err))

{-|
  Get a list of all documents of a given DocType including all fields.

  By default, only the @name@ field is fetched. See 'getDocList' for more details.

  System fields like @_assign@ and @_user_tags@ are not
  included, they are not part of the DocType definition.
-}
getDocListAllFields :: forall a. (IsDocType a, FromJSON a)
               => Manager
               -> Config
               -> [QueryStringParam]
               -> IO (ApiResponse [Fieldname], Maybe (ApiResponse [a]))
getDocListAllFields manager config qsParams = do
  getAllFieldnames @a manager config
    `andThen`
    (\fieldnames -> getDocList manager config $ Fields fieldnames : filter noFields qsParams)
  where
    noFields (Fields _) = False
    noFields _ = True

-- | Get a single document of a given DocType by name.
getDoc :: forall a. (IsDocType a, FromJSON a)
           => Manager -> Config -> Text -> IO (ApiResponse a)
getDoc manager config name = do
  response <- Simple.getDoc manager config (docTypeName @a) name
  return $ parseTypedResponse response

{- | Delete a single document of a given DocType by name.

The phantom type parameter @a@ is used to figure out the DocType.
A customer can be deleted like this:

@
res \<- deleteDoc @Customer manager config "customer name"
@
-}
deleteDoc :: forall a. (IsDocType a)
              => Manager -> Config -> Text -> IO (ApiResponse ())
deleteDoc manager config name =
  Simple.deleteDoc manager config (docTypeName @a) name

-- | Create a new document of a given DocType.
postDoc :: forall a. (IsDocType a, FromJSON a, ToJSON a)
            => Manager -> Config -> a -> IO (ApiResponse a)
postDoc manager config doc = do
  response <- Simple.postDoc manager config (docTypeName @a) (toJSON doc)
  return $ parseTypedResponse response

-- | Update a document of a given DocType by name.
putDoc :: forall a. (IsDocType a, FromJSON a, ToJSON a)
           => Manager -> Config -> Text -> a -> IO (ApiResponse a)
putDoc manager config name doc = do
  response <- Simple.putDoc manager config (docTypeName @a) name (toJSON doc)
  return $ parseTypedResponse response

-- | Get the full response from the API response.
getResponse :: ApiResponse a -> Response LBS.ByteString
getResponse (Ok r _ _) = r
getResponse (Err r _) = r

-- Helper function to convert ApiResponse Value to ApiResponse a
parseTypedResponse :: FromJSON a => ApiResponse Value -> ApiResponse a
parseTypedResponse (Ok response val jsonVal) =
  case fromJSON jsonVal of
    Success a -> Ok response val a
    Error err -> Err response (Just (val, pack err))
parseTypedResponse (Err response err) = Err response err

{-|
This function helps to sequentially execute two API calls A and B by
passing some part of A's result to B. This can reduce the number of case
expressions to one.

@
  let andThen = andThenWith customer
  (salesOrder', mCustomer') <- getDoc @SalesOrder mgr cfg "SAL-ORD-2025-00031"
                                `andThen` getDoc @Customer mgr cfg
@

It can easily combine: TODO:untested

- 'getDoc' and 'getDoc'
- 'getDoc' and 'deleteDoc'

But it can also combine all other API calls with a proper
transformation @f@ and the use of 'mapM' for wrapping B.

To use it as infix operator, you need to apply the transformation
function @f@ to 'andThenWith' and bind that to a name like
@andThenWithCustomerField@.

Examples: TODO:untested

@
  -- delete all Sales Orders of Customer
  getDoc @Customer mgr cfg "Company A" `andThenWithTheirSalesOrders` mapM (deleteDoc @SalesOrder mgr cfg)

  -- update all Customer matching query
  getDocList @Customer mgr cfg query `andThenWithCustomerName` mapM_ (\(name, updatedDoc) -> putDoc @Customer mgr cfg name updatedDoc)
@

-}
andThenWith
  :: (a -> c)
  -> IO (ApiResponse a)
  -> (c -> IO (ApiResponse b))
  -> IO (ApiResponse a, Maybe (ApiResponse b))
andThenWith f io1 io2 = do
  res1 <- io1
  case res1 of
    (Err _ _) -> return (res1, Nothing)
    (Ok _ _ x) -> do
      res <- io2 (f x)
      return (res1, Just res)

andThen
  :: IO (ApiResponse a)
  -> (a -> IO (ApiResponse b))
  -> IO (ApiResponse a, Maybe (ApiResponse b))
andThen = andThenWith id

data DocType = DocType
  { dtName :: Text
  , dtFieldnames :: [Text]
  }
  deriving (Show)

instance FromJSON DocType where
  parseJSON = withObject "DocType" $ \obj -> do
    name <- obj .: "name"
    fieldsArray <- obj .: "fields"
    fieldNames <- mapM extractFieldName fieldsArray
    return $ DocType name fieldNames
    where
      extractFieldName = withObject "Field" $ \fieldObj ->
        fieldObj .: "fieldname"

instance IsDocType DocType where
  docTypeName = "DocType"
  docName = dtName

getAllFieldnames :: forall a. (IsDocType a)
               => Manager
               -> Config
               -> IO (ApiResponse [Fieldname])
getAllFieldnames manager config = do
  res <- getDoc @DocType manager config (docTypeName @a)
  return $ dtFieldnames <$> res

-- | Fixed ERPNext system field names that all DocTypes have in common.
--
-- These are not included in 'getAllFieldnames's response.
systemFieldnames :: [Fieldname]
systemFieldnames =
  [ "_assign"
  , "_user_tags"
  , "_liked_by"
  , "_comments"
  ]
