{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Simple API client for ERPNext with minimal type requirements

This module provides basic API functions that work with JSON Values directly
and take DocType names as Text arguments, making it easier to use when you
don't want to define custom types.
-}

module ERPNext.Client.Simple
  ( getDocList
  , getDoc
  , postDoc
  , putDoc
  , getMethodCall
  , postMethodCall
  , deleteDoc
  , mkSecret
  , mkConfig
  , getResponse
  , showJsonResponsePretty
  , showApiResponseDebug
  , uploadFile
  , ApiResponse (..)
  , Config
  , Secret
  ) where

import Network.HTTP.Client (Manager, httpLbs, Response (..), Request (..), parseRequest, RequestBody (..), setQueryString)
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody, partBS)
import Network.HTTP.Types (hAuthorization, hContentType, Header, statusCode, statusMessage)
import Data.ByteString.Char8 qualified as BS8
import Data.Text hiding (show, length, concatMap, null, map)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (Value (..), FromJSON (..), Result (..), fromJSON, decode, encode, ToJSON, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import ERPNext.Client.Helper (urlEncode, showJsonPretty)

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

-- | Create an API client configuration.
mkConfig
  :: Text -- ^ The API base URL, e.g. @https://erpnext.example.com/api"@.
  -> Text -- ^ The API key.
  -> Secret -- ^ The API secret.
  -> Config
mkConfig baseUrl apiKey apiSecret = Config
  { baseUrl = baseUrl
  , apiKey = apiKey
  , apiSecret = apiSecret
  }

-- | Create the API secret used together with the API key for authorization.
mkSecret :: Text -> Secret
mkSecret = Secret

-- | Data wrapper type just to parse the JSON returned by ERPNext.
data DataWrapper a = DataWrapper { getData :: a }
  deriving Show

instance FromJSON a => FromJSON (DataWrapper a) where
  parseJSON = withObject "DataWrapper" $ \obj -> do
    dataValue <- obj .: "data"
    return (DataWrapper dataValue)

-- | Message wrapper type to parse JSON returned by ERPNext remote method calls.
data MessageWrapper a = MessageWrapper { getMessage :: a }
  deriving Show

instance FromJSON a => FromJSON (MessageWrapper a) where
  parseJSON = withObject "MessageWrapper" $ \obj -> do
    messageValue <- obj .: "message"
    return (MessageWrapper messageValue)

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

-- | Pretty-print JSON API response or print HTTP status and message
-- if response is no valid JSON.
showJsonResponsePretty :: ApiResponse a -> String
showJsonResponsePretty (Ok _ val _) = showJsonPretty val
showJsonResponsePretty (Err _ (Just (val, _))) = showJsonPretty val
showJsonResponsePretty (Err _ Nothing) = "{}"

showApiResponseDebug :: ApiResponse a -> String
showApiResponseDebug response =
  "HTTP " ++ show (statusCode status) ++ " "
   ++ BS8.unpack (statusMessage status)
   ++ "\n\n"
   ++ body
  where
    status = responseStatus $ getResponse response
    body = case response of
      (Ok _ val _) -> showJsonPretty val
      (Err _ (Just (val, _))) ->
        let mainOutput = showJsonPretty val
            excOutput = maybe "" ("\n\nParsed \"exc\" field:\n" ++) $ extractExceptionTraceback val
        in mainOutput ++ excOutput
      (Err _ Nothing) -> "No valid JSON response."

-- | Extract and parse the "exc" field content from a JSON object.
extractExceptionTraceback :: Value -> Maybe String
extractExceptionTraceback (Object obj) = do
  String excStr <- KeyMap.lookup "exc" obj
  Array arr <- decode @Value $ LBS.fromStrict $ encodeUtf8 excStr
  let content = concatMap extractStringContent arr
  if null content then Nothing else Just content
  where
    extractStringContent (String s) = unpack s
    extractStringContent _ = ""
extractExceptionTraceback _ = Nothing

-- | Get the full response from the API response.
getResponse :: ApiResponse a -> Response LBS.ByteString
getResponse (Ok r _ _) = r
getResponse (Err r _) = r

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

parseMethodResponse :: forall a. FromJSON a => Response LBS.ByteString -> ApiResponse a
parseMethodResponse response =
  case decode @Value (responseBody response) of
    Just value -> case fromJSON value :: Result (MessageWrapper a) of
      Success result -> Ok response value (getMessage result)
      Error err -> Err response (Just (value, pack err))
    Nothing -> Err response Nothing

-- | Get a list of documents for a given DocType name.
--
-- The passed query string is used as is. To properly encode it, use
-- 'ERPNext.Helper.urlEncodeQueryString'.
-- It's a bit tricky because both, ERPNext and the http-client package
-- have certain expectations on how the query string must be encoded.
--
-- @
-- res <- getDocList manager config "Customer" (Just $ urlEncodeQueryString "fields=[\"customer_name\"]&page_limit_length=10")
-- @
--
getDocList :: Manager
           -> Config
           -> Text -- ^ DocType name
           -> Maybe Text -- ^ Optional query string filter (used in URL as is)
           -> IO (ApiResponse [Value])
getDocList manager config docTypeName mFilter = do
  let path = "/resource/" <> urlEncode docTypeName <> maybe "" ("?" <>) mFilter
  request <- createRequest config path "GET"
  response <- httpLbs request manager
  return $ parseGetResponse response

-- | Get a single document by DocType name and document name.
getDoc :: Manager
       -> Config
       -> Text -- ^ DocType name
       -> Text -- ^ Document name
       -> IO (ApiResponse Value)
getDoc manager config docTypeName docName = do
  let path = "/resource/" <> urlEncode docTypeName <> "/" <> urlEncode docName
  request <- createRequest config path "GET"
  response <- httpLbs request manager
  return $ parseGetResponse response

-- | Create a new document.
postDoc :: Manager
        -> Config
        -> Text -- ^ DocType name
        -> Value -- ^ Document data as JSON
        -> IO (ApiResponse Value)
postDoc manager config docTypeName docData = do
  let path = "/resource/" <> urlEncode docTypeName
  request <- createRequestWithBody config path "POST" docData
  response <- httpLbs request manager
  return $ parseGetResponse response

-- | Update an existing document.
putDoc :: Manager
       -> Config
       -> Text -- ^ DocType name
       -> Text -- ^ Document name
       -> Value -- ^ Updated document data as JSON
       -> IO (ApiResponse Value)
putDoc manager config docTypeName docName docData = do
  let path = "/resource/" <> urlEncode docTypeName <> "/" <> urlEncode docName
  request <- createRequestWithBody config path "PUT" docData
  response <- httpLbs request manager
  return $ parseGetResponse response

-- | Delete a document.
deleteDoc :: Manager
          -> Config
          -> Text -- ^ DocType name
          -> Text -- ^ Document name
          -> IO (ApiResponse ())
deleteDoc manager config docTypeName docName = do
  let path = "/resource/" <> urlEncode docTypeName <> "/" <> urlEncode docName
  request <- createRequest config path "DELETE"
  response <- httpLbs request manager
  return $ parseDeleteResponse response

-- | Helper function for method calls that handles the common logic.
remoteMethodCall :: Manager
           -> Config
           -> Text -- ^ Method name
           -> BS.ByteString -- ^ HTTP method (GET or POST)
           -> [(Text, Maybe Text)] -- ^ Parameters
           -> IO (ApiResponse Value)
remoteMethodCall manager config methodName httpMethod args = do
  let path = "/method/" <> urlEncode methodName
  request <- createRequest config path httpMethod
  let args' = map (\(x,y) -> (encodeUtf8 x, encodeUtf8 <$> y)) args
  let requestWithArgs = setQueryString args' request
  response <- httpLbs requestWithArgs manager
  return $ parseMethodResponse response

-- | Read-only remote method call using HTTP GET.
getMethodCall
          :: Manager
          -> Config
          -> Text -- ^ Method name, e.g. @frappe.auth.get_logged_user@.
          -> [(Text, Maybe Text)] -- ^ Parameters for remote method call, passed as query string.
          -> IO (ApiResponse Value)
getMethodCall manager config methodName args =
  remoteMethodCall manager config methodName "GET" args

-- | Remote method call using HTTP POST that can modify state on ERPNext.
postMethodCall
          :: Manager
          -> Config
          -> Text -- ^ Method name, e.g. @frappe.client.submit_doc@.
          -> [(Text, Maybe Text)] -- ^ Parameters for remote method call, passed as query string.
          -> IO (ApiResponse Value)
postMethodCall manager config methodName args =
  remoteMethodCall manager config methodName "POST" args

 -- | Uploads a file and attaches it to an existing document.
uploadFile
  :: Manager
  -> Config
  -> Text          -- ^ DoctType
  -> Text          -- ^ DocName
  -> Text          -- ^ File name, e.g. "img.jpg"
  -> LBS.ByteString -- ^ Raw file contents
  -> IO (ApiResponse Value)
uploadFile manager config doctype docname fileName fileContents = do
  request <- createRequest config "/method/upload_file" "POST"
  requestWithBody <- formDataBody
    [ partFileRequestBody "file" (unpack fileName) (RequestBodyLBS fileContents)
    , partBS "is_private" "1"
    , partBS "doctype" (encodeUtf8 doctype)
    , partBS "docname" (encodeUtf8 docname)
    ]
    request
  response <- httpLbs requestWithBody manager
  return $ parseMethodResponse response
