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
import Network.HTTP.Types.Header (hAuthorization, Header)
import Data.Text hiding (map)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Data.Proxy
import Data.ByteString.Lazy (ByteString)
import ERPNext.Client.QueryStringParams
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
deleteDocType :: forall a. (IsDocType a) => Manager -> Config -> Text -> IO (ApiResponse Bool)
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
  request <- createRequestWithBody config path doc "PUT"
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
createRequest :: Config -> Text -> Text -> IO Request
createRequest config path method = do
  request <- parseRequest $ unpack (baseUrl config <> path)
  return request
    { method = encodeUtf8 method
    , requestHeaders = mkHeader config
    }

createRequestWithBody :: (ToJSON a) => Config -> Text -> a -> Text -> IO Request
createRequestWithBody config path doc method = do
  request <- parseRequest $ unpack (baseUrl config <> path)
  return request
    { method = encodeUtf8 method
    , requestHeaders = mkHeader config
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

-- TODO: the response should also contain: the full JSON Value
-- (esp. in the error case), the raw response in case the response
-- cannot be parsed, the http response incl. http status code (this
-- probably makes the raw text accessible)
data ApiResponse a =
    Ok
     { getResponse :: Response ByteString
     , getResult :: a
     , getJsonValue :: Value
     }
  | Err
     { getResponse :: Response ByteString
     , getMaybeJsonValueErr :: Maybe Value
     }
  deriving Show

mkHeader :: Config -> [Header]
mkHeader config = let authToken = apiKey config <> ":" <> getSecret (apiSecret config)
                          in [(hAuthorization, encodeUtf8 $ "token " <> authToken), ("Content-Type", "application/json")]

parseGetResponse :: forall a. FromJSON a => Response ByteString -> ApiResponse a
parseGetResponse response =
  case eitherDecode @Value (responseBody response) of
    Left _ -> Err response Nothing
    Right value -> case fromJSON value :: Result (DataWrapper a) of
      Success result -> Ok response (getData result) value
      Error err -> Err response Nothing

parseDeleteResponse :: Response ByteString -> ApiResponse Bool
parseDeleteResponse response = 
  case decode (responseBody response) :: Maybe Value of
    Just res -> if res == object ["data" .= ("ok" :: Text)] then Ok response True res
                else Err response (Just res)
    Nothing -> Err response Nothing

getResourcePath :: forall a. IsDocType a => Proxy a -> Text
getResourcePath _ = "/resource/" <> docTypeName @a
