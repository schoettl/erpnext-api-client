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
import ERPNext.Client.Filters (Filters, makeFiltersText)
import Prelude

-- | Type class for types which represent an ERPNext DocType.
-- Each DocType has a unique name.
class IsDocType a where
  docTypeName :: Text

getDocTypeList :: forall a. (IsDocType a, FromJSON a) => Manager -> Config  -> [QueryStringParam]-> IO (ApiResponse [a])
getDocTypeList manager config qsParams = do
  initialRequest <- parseRequest $ unpack $ baseUrl config <> "/resource/" <> docTypeName @a <> "?" <> renderQueryStringParams qsParams
  let request = initialRequest
                  { method = "GET"
                  , requestHeaders = [mkAuthHeader config]
                  }
  response <- Network.HTTP.Client.httpLbs request manager
  let value = decode (responseBody response) :: Maybe Value
  let result = decode (responseBody response) :: Maybe (DataWrapper [a])

  return $ case (result, value) of
      (Just res, Just val) -> Ok response (getData res) val
      (Nothing, Just val) -> Err response (Just val)
      (Nothing, Nothing) -> Err response Nothing

getDocType :: forall a. (IsDocType a, FromJSON a) => Manager -> Config -> Text -> IO (ApiResponse a)
getDocType manager config id = do
    initialRequest <- parseRequest $ unpack $ baseUrl config <> "/resource/" <> docTypeName @a <> "/" <> id
    let request = initialRequest
                    { method = "GET"
                    , requestHeaders = [mkAuthHeader config]
                    }
    response <- Network.HTTP.Client.httpLbs request manager
    let value = decode (responseBody response) :: Maybe Value
    let result = decode (responseBody response) :: Maybe (DataWrapper a)

    return $ case (result, value) of
        (Just res, Just val) -> Ok response (getData res) val
        (Nothing, Just val) -> Err response (Just val)
        (Nothing, Nothing) -> Err response Nothing

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
      makeFiltersText filters


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
     , getJsonValueErr :: Maybe Value
     }
  deriving Show

mkAuthHeader :: Config -> Header
mkAuthHeader config = let authToken = apiKey config <> ":" <> getSecret (apiSecret config)
                          in (hAuthorization, encodeUtf8 $ "token " <> authToken)
