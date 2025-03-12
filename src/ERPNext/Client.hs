module ERPNext.Client
  ( getDocTypeList
  , getDocType
  , postDocType
  , putDocType
  , deleteDocType
  , mkConfig
  , mkSecret
  , Config ()
  , Secret ()
  ) where

import Network.HTTP.Client
import Data.Text

getDocTypeList :: Config -> IO [a]
getDocTypeList _ = error "implement"

getDocType :: Config -> Text -> IO (Result a)
getDocType _ _ = error "implement"

deleteDocType :: Config -> Text -> IO (Result a) -- ??
deleteDocType _ _ = error "implement"

postDocType :: Config -> Text -> a -> IO (Result a)
postDocType _ _ _ = error "implement"

putDocType :: Config -> Text -> a -> IO (Result a)
putDocType _ _ _ = error "implement"

mkConfig :: Text -> Text -> Secret -> Config
mkConfig baseUrl apiUser apiSecret = Config
  { baseUrl = baseUrl
  , apiUser = apiUser
  , apiSecret = apiSecret
  , tlsSettings = defaultManagerSettings
  }

mkSecret :: Text -> Secret
mkSecret = Secret

data Config = Config
  { baseUrl :: Text
  , apiUser :: Text
  , apiSecret :: Secret
  , tlsSettings :: ManagerSettings
  }

data Secret = Secret
  { getSecret :: Text
  }

data Result a = Ok a | Error Text
