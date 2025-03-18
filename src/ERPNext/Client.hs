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

import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization, Header)
import Data.Text hiding (map)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Data.Proxy qualified
import Data.ByteString.Lazy (ByteString)
import ERPNext.Client.Filters (Filters, renderFilters)
import Prelude

-- | Type class for types which represent an ERPNext DocType.
-- Each DocType has a unique name.
class IsDocType a where
  docTypeName :: Text

getDocTypeList :: forall a. (IsDocType a, FromJSON a) => Manager -> Config  -> [QueryStringParam]-> IO (ApiResponse [a])
getDocTypeList manager config qsParams = do
  request <- createRequest config ("/resource/" <> docTypeName @a <> "?" <> renderQueryStringParams qsParams) "GET"
  response <- Network.HTTP.Client.httpLbs request manager
  let body = responseBody response
  return $ case eitherDecode body :: Either String Value of
    Left _ -> Err response Nothing
    Right value -> case (fromJSON value) :: Result (DataWrapper [a]) of
      Success result -> Ok response (getData result) value
      Error err -> Err response Nothing

getDocType :: forall a. (IsDocType a, FromJSON a) => Manager -> Config -> Text -> IO (ApiResponse a)
getDocType manager config id = do
  request <- createRequest config ("/resource/" <> docTypeName @a <> "/" <> id) "GET"
  response <- Network.HTTP.Client.httpLbs request manager
  let body = responseBody response
  return $ case eitherDecode body :: Either String Value of
    Left _ -> Err response Nothing
    Right value -> case (fromJSON value) :: Result (DataWrapper a) of
      Success result -> Ok response (getData result) value
      Error err -> Err response Nothing

{- | Delete a named object.

The 'Data.Prox.Proxy' parameter is used to figure out the DocType.
A customer can be deleted like this:

@
res <- deleteDocType config "<customer name>" (Proxy :: Proxy Customer)
@
-}
deleteDocType :: forall a. (IsDocType a)
              => Config -> Text -> Data.Proxy.Proxy a -> IO (ApiResponse Bool)
deleteDocType _ _ _ = error "implement"
-- note: return type is actually just {"message":"ok"}

postDocType :: forall a. (IsDocType a, FromJSON a, ToJSON a)
            => Config -> a -> IO (ApiResponse a)
postDocType _ _ = error "implement"

putDocType :: forall a. (IsDocType a, FromJSON a, ToJSON a)
           => Config -> Text -> a -> IO (ApiResponse a)
putDocType _ _ _ = error "implement"

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
    , requestHeaders = [mkAuthHeader config]
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

-- TODO: Placeholder
-- TODO: Maybe rename type to make it more abstract (not tied to the URL query string)?
-- TODO: Maybe change type or make opaque type to prevent invalid combinations?
data QueryStringParam = Asc Text | Desc Text | Fields [Text] | F Filters

renderQueryStringParam :: QueryStringParam -> Text
renderQueryStringParam qsParam = 
  case qsParam of
    Asc _ ->
      ""
    Desc _ ->
      ""
    Fields _ ->
      ""
    F filters ->
      renderFilters filters


renderQueryStringParams :: [QueryStringParam] -> Text
renderQueryStringParams params = intercalate "&" (map renderQueryStringParam params)



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

mkAuthHeader :: Config -> Header
mkAuthHeader config = let authToken = apiKey config <> ":" <> getSecret (apiSecret config)
                          in (hAuthorization, encodeUtf8 $ "token " <> authToken)
