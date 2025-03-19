{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

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
  ) where

import Network.HTTP.Client (Response (..), Request (..), Manager, httpLbs, parseRequest, RequestBody (..))
import Network.HTTP.Types (hAuthorization, hContentType, Header)
import Data.Text hiding (map)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Data.Proxy
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import ERPNext.Client.QueryStringParams
import ERPNext.Client.Helper (urlEncode)
import Prelude

-- | Type class for types which represent an ERPNext DocType.
-- Each DocType has a unique name.
class IsDocType a where
  docTypeName :: Text

getDocTypeList :: forall a. (IsDocType a, FromJSON a) => Manager -> Config  -> [QueryStringParam]-> IO (ApiResponse [a])
getDocTypeList manager config qsParams = do
  request <- createRequest config (getResourcePath (Proxy @a) <> "?" <> renderQueryStringParams qsParams) "GET"
  response <- Network.HTTP.Client.httpLbs request manager
  return $ parseGetResponse response

getDocType :: forall a. (IsDocType a, FromJSON a) => Manager -> Config -> Text -> IO (ApiResponse a)
getDocType manager config id = do
  request <- createRequest config (getResourcePath (Proxy @a) <> "/" <> id) "GET"
  response <- Network.HTTP.Client.httpLbs request manager
  return $ parseGetResponse response

{- | Delete a named object.

The 'Data.Prox.Proxy' parameter is used to figure out the DocType.
A customer can be deleted like this:

@
res <- deleteDocType config "<customer name>" (Proxy :: Proxy Customer)
@
-}
deleteDocType :: forall a. (IsDocType a) => Manager -> Config -> Text -> IO (ApiResponse ())
deleteDocType manager config name = do
  request <- createRequest config (getResourcePath (Proxy @a) <> "/" <> name) "DELETE"
  response <- Network.HTTP.Client.httpLbs request manager
  return $ parseDeleteResponse response

postDocType :: forall a. (IsDocType a, FromJSON a, ToJSON a)
            => Config -> a -> IO (ApiResponse a)
postDocType _ _ = error "implement"

putDocType :: forall a. (IsDocType a, FromJSON a, ToJSON a) => Manager -> Config -> Text -> a -> IO (ApiResponse a)
putDocType manager config name doc = do
  let path = getResourcePath (Proxy @a) <> "/" <> name
  request <- createRequestWithBody config path "PUT" doc
  response <- Network.HTTP.Client.httpLbs request manager
  return $ parseGetResponse response


mkConfig :: Text -> Text -> Secret -> Config
mkConfig baseUrl apiKey apiSecret = Config
  { baseUrl = baseUrl
  , apiKey = apiKey
  , apiSecret = apiSecret
  }

-- | Create the API secret used together with the API key for authorization.
mkSecret :: Text -> Secret
mkSecret = Secret


-- | Create the API Request.
createRequest :: Config -> Text -> BS.ByteString -> IO Request
createRequest config path method = do
  request <- parseRequest $ unpack (baseUrl config <> path)
  return request
    { method = method
    , requestHeaders = [mkAuthHeader config]
    }

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

data DataWrapper a = DataWrapper { getData :: a }
  deriving Show

instance FromJSON a => FromJSON (DataWrapper a) where
  parseJSON = withObject "DataWrapper" $ \obj -> do
    dataValue <- obj .: "data"
    return (DataWrapper dataValue)

data ApiResponse a =
    Ok
     { getResponse :: Response LBS.ByteString
     , getResult :: a
     , getJsonValue :: Value
     }
  | Err
     { getResponse :: Response LBS.ByteString
     , getMaybeJsonValue :: Maybe (Value, Text)
     }
  deriving Show

mkAuthHeader :: Config -> Header
mkAuthHeader config = let authToken = apiKey config <> ":" <> getSecret (apiSecret config)
                          in (hAuthorization, encodeUtf8 $ "token " <> authToken)

parseGetResponse :: forall a. FromJSON a => Response LBS.ByteString -> ApiResponse a
parseGetResponse response =
  case decode @Value (responseBody response) of
    Just value -> case fromJSON value :: Result (DataWrapper a) of
      Success result -> Ok response (getData result) value
      Error err -> Err response (Just (value, pack err))
    Nothing -> Err response Nothing

parseDeleteResponse :: Response LBS.ByteString -> ApiResponse ()
parseDeleteResponse response =
  case decode @Value (responseBody response) of
    Just value -> case fromJSON value :: Result (DataWrapper Text) of
      Success (DataWrapper message)
        | message == "ok" -> Ok response () value
        | otherwise -> Err response (Just (value, message))
      Error err -> Err response (Just (value, pack err))
    Nothing -> Err response Nothing

getResourcePath :: forall a. IsDocType a => Proxy a -> Text
getResourcePath _ = "/resource/" <> urlEncode (docTypeName @a)
