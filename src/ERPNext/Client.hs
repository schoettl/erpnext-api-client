{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module ERPNext.Client
  ( getDocTypeList
  , getDocType
  , postDocType
  , putDocType
  , deleteDocType
  , mkSecret
  , mkConfig
  , withTlsSettings
  , IsDocType (..)
  , Config ()
  , Secret ()
  , QueryStringParam (..)
  , ApiResponse (..)
  ) where

import Network.HTTP.Client
import Data.Text
import Data.Aeson
import Data.Proxy qualified

-- | Type class for types which represent an ERPNext DocType.
-- Each DocType has a unique name.
class IsDocType a where
  docTypeName :: Text
  -- TODO: implement auto-derive (using typename and generic)?

getDocTypeList :: forall a. (IsDocType a, FromJSON a) => Config -> Filters -> ApiResponse [a]
getDocTypeList = error (unpack $ docTypeName @a)

getDocType :: forall a. (IsDocType a, FromJSON a) => Config -> Text -> IO (ApiResponse a)
getDocType _ _ = error "implement"

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
  , tlsSettings = defaultManagerSettings
  }

-- | Update 'Config' and set user-provided TLS settings.
withTlsSettings :: Config -> ManagerSettings -> Config
withTlsSettings c x = c { tlsSettings = x }

-- | Create the API secret used together with the API key for authorization.
mkSecret :: Text -> Secret
mkSecret = Secret

-- | API client configuration.
data Config = Config
  { baseUrl :: Text
  , apiKey :: Text
  , apiSecret :: Secret
  , tlsSettings :: ManagerSettings
  }

-- | Opaque type to store the API secret.
data Secret = Secret
  { getSecret :: Text
  }

-- TODO: Placeholder
-- TODO: Maybe rename type to make it more abstract (not tied to the URL query string)?
-- TODO: Maybe change type or make opaque type to prevent invalid combinations?
data QueryStringParam = Asc Text | Desc Text | Fields [Text]

-- TODO: the response should also contain: the full JSON Value
-- (esp. in the error case), the raw response in case the response
-- cannot be parsed, the http response incl. http status code (this
-- probably makes the raw text accessible)
data ApiResponse a = Ok a Value | Error HttpResponse (Maybe Value)
