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
  , ApiResponse (..)
  ) where

import Network.HTTP.Client
import Data.Text
import Data.Aeson

-- | Type class for types which represent an ERPNext DocType.
-- Each DocType has a unique name.
class IsDocType a where
  docTypeName :: Text

getDocTypeList :: forall a. (IsDocType a, FromJSON a) => Config -> ApiResponse [a]
getDocTypeList = error (unpack $ docTypeName @a)

getDocType :: Config -> Text -> IO (ApiResponse a)
getDocType _ _ = error "implement"

deleteDocType :: Config -> Text -> IO (ApiResponse a) -- ??
deleteDocType _ _ = error "implement"

postDocType :: Config -> Text -> a -> IO (ApiResponse a)
postDocType _ _ _ = error "implement"

putDocType :: Config -> Text -> a -> IO (ApiResponse a)
putDocType _ _ _ = error "implement"

mkConfig :: Text -> Text -> Secret -> Config
mkConfig baseUrl apiKey apiSecret = Config
  { baseUrl = baseUrl
  , apiKey = apiKey
  , apiSecret = apiSecret
  , tlsSettings = defaultManagerSettings
  }

withTlsSettings :: Config -> ManagerSettings -> Config
withTlsSettings c x = c { tlsSettings = x }

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

data ApiResponse a = Ok a | Error Text
