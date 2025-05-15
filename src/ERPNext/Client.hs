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
  ( getDocTypeList
  , getDocType
  , postDocType
  , putDocType
  , deleteDocType
  , mkSecret
  , mkConfig
  , IsDocType (..)
  , Config ()
  , Secret ()
  , QueryStringParam (..)
  , ApiResponse (..)
  , getResponse
  , andThenWith
  ) where

import Network.HTTP.Client (Response (..), Request (..), Manager, httpLbs, parseRequest, RequestBody (..))
import Network.HTTP.Types (hAuthorization, hContentType, Header)
import Data.Text hiding (map)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import ERPNext.Client.QueryStringParams
import ERPNext.Client.Helper (urlEncode)

-- | Type class for types which represent an ERPNext DocType.
-- Each DocType has a unique name but there can still be multiple
-- „views“ (i.e. records types) for one DocType.
class IsDocType a where
  docTypeName :: Text

{-|
  Get a list of all documents of a given DocType.
  The 'QueryStringParam's can select fields, filter, order, enable
  paging, and more.
-}
getDocTypeList :: forall a. (IsDocType a, FromJSON a)
               => Manager -> Config  -> [QueryStringParam]-> IO (ApiResponse [a])
getDocTypeList manager config qsParams = do
  let path = getResourcePath @a <> "?" <> renderQueryStringParams qsParams
  request <- createRequest config path "GET"
  response <- httpLbs request manager
  return $ parseGetResponse response

-- | Get a single document of a given DocType by name.
getDocType :: forall a. (IsDocType a, FromJSON a)
           => Manager -> Config -> Text -> IO (ApiResponse a)
getDocType manager config name = do
  let path = getResourcePath @a <> "/" <> name
  request <- createRequest config path "GET"
  response <- httpLbs request manager
  return $ parseGetResponse response

{- | Delete a single document of a given DocType by name.

The phantom type parameter @a@ is used to figure out the DocType.
A customer can be deleted like this:

@
res \<- deleteDocType @Customer manager config "customer name"
@
-}
deleteDocType :: forall a. (IsDocType a)
              => Manager -> Config -> Text -> IO (ApiResponse ())
deleteDocType manager config name = do
  let path = getResourcePath @a <> "/" <> name
  request <- createRequest config path "DELETE"
  response <- httpLbs request manager
  return $ parseDeleteResponse response

-- | Create a new document of a given DocType.
postDocType :: forall a. (IsDocType a, FromJSON a, ToJSON a)
            => Manager -> Config -> a -> IO (ApiResponse a)
postDocType manager config doc = do
  let path = getResourcePath @a
  request <- createRequestWithBody config path "POST" doc
  response <- httpLbs request manager
  return $ parseGetResponse response

-- | Update a document of a given DocType by name.
putDocType :: forall a. (IsDocType a, FromJSON a, ToJSON a)
           => Manager -> Config -> Text -> a -> IO (ApiResponse a)
putDocType manager config name doc = do
  let path = getResourcePath @a <> "/" <> name
  request <- createRequestWithBody config path "PUT" doc
  response <- httpLbs request manager
  return $ parseGetResponse response

-- | Create an API client configuration.
mkConfig :: Text -> Text -> Secret -> Config
mkConfig baseUrl apiKey apiSecret = Config
  { baseUrl = baseUrl
  , apiKey = apiKey
  , apiSecret = apiSecret
  }

-- | Create the API secret used together with the API key for authorization.
mkSecret :: Text -> Secret
mkSecret = Secret


-- | Create the API 'Request'.
createRequest :: Config -> Text -> BS.ByteString -> IO Request
createRequest config path method = do
  request <- parseRequest $ unpack (baseUrl config <> path)
  return request
    { method = method
    , requestHeaders = [mkAuthHeader config]
    }

-- | Create the API 'Request' with a JSON body.
createRequestWithBody :: ToJSON a => Config -> Text -> BS.ByteString -> a -> IO Request
createRequestWithBody config path method doc = do
  request <- parseRequest $ unpack (baseUrl config <> path)
  return request
    { method = method
    , requestHeaders = mkAuthHeader config : [(hContentType, encodeUtf8 "application/json")]
    , requestBody = RequestBodyLBS (encode doc)
    }

-- | API client configuration.
data Config = Config
  { baseUrl :: Text
  , apiKey :: Text
  , apiSecret :: Secret
  }

-- | Opaque type to store the API secret.
data Secret = Secret
  { getSecret :: Text
  }

-- | Data wrapper type just to parse the JSON returned by ERPNext.
data DataWrapper a = DataWrapper { getData :: a }
  deriving Show

instance FromJSON a => FromJSON (DataWrapper a) where
  parseJSON = withObject "DataWrapper" $ \obj -> do
    dataValue <- obj .: "data"
    return (DataWrapper dataValue)

-- | The API response.
data ApiResponse a
  = Ok -- ^ The OK response.
      (Response LBS.ByteString) -- ^ The server's full response including header information.
      Value -- ^ The returned JSON.
      a -- ^ The result parsed from the returned JSON.
  | Err -- ^ The error response.
      (Response LBS.ByteString) -- ^ The server's full response including header information.
      (Maybe (Value, Text)) -- ^ If the response is valid JSON, 'Just' the returned JSON and
                            -- the parse error message telling why 'Value' couldn't be parsed
                            -- into @a@.
  deriving Show

instance Functor ApiResponse where
  fmap f (Ok response val x) = Ok response val (f x)
  fmap _ (Err response err)  = Err response err


-- | Get the full response from the API response.
getResponse :: ApiResponse a -> Response LBS.ByteString
getResponse (Ok r _ _) = r
getResponse (Err r _) = r

mkAuthHeader :: Config -> Header
mkAuthHeader config = let authToken = apiKey config <> ":" <> getSecret (apiSecret config)
                          in (hAuthorization, encodeUtf8 $ "token " <> authToken)

parseGetResponse :: forall a. FromJSON a => Response LBS.ByteString -> ApiResponse a
parseGetResponse response =
  case decode @Value (responseBody response) of
    Just value -> case fromJSON value :: Result (DataWrapper a) of
      Success result -> Ok response value (getData result)
      Error err -> Err response (Just (value, pack err))
    Nothing -> Err response Nothing

parseDeleteResponse :: Response LBS.ByteString -> ApiResponse ()
parseDeleteResponse response =
  case decode @Value (responseBody response) of
    Just value -> case fromJSON value :: Result (DataWrapper Text) of
      Success (DataWrapper message)
        | message == "ok" -> Ok response value ()
        | otherwise -> Err response (Just (value, message))
      Error err -> Err response (Just (value, pack err))
    Nothing -> Err response Nothing

getResourcePath :: forall a. IsDocType a => Text
getResourcePath = "/resource/" <> urlEncode (docTypeName @a)

{-|
This function helps to sequentially execute two API calls A and B by
passing some part of A's result to B. This can reduce the number of case
expressions to one.

@
  TODO:untested
  let andThen = andThenWith customer
  (salesOrder', mCustomer') <- getDocType @SalesOrder mgr cfg "SAL-ORD-2025-00031"
                                `andThen` getDocType @Customer mgr cfg
@

It can easily combine: TODO:untested

- 'getDocType' and 'getDocType'
- 'getDocType' and 'deleteDocType'

But it can also combine all other API calls with a proper
transformation @f@ and the use of 'mapM' for wrapping B.

To use it as infix operator, you need to apply the transformation
function @f@ to 'andThenWith' and bind that to a name like
@andThenWithCustomerField@.

Examples: TODO:untested

@
  -- delete all Sales Orders of Customer
  getDocType @Customer mgr cfg "Company A" `andThenWithTheirSalesOrders` mapM (deleteDocType @SalesOrder mgr cfg)

  -- update all Customer matching query
  getDocTypeList @Customer mgr cfg query `andThenWithCustomerName` mapM_ (\(name, updatedDoc) -> putDocType @Customer mgr cfg name updatedDoc)
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
