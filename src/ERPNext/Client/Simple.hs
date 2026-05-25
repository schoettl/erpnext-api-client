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
  , deleteDoc
  ) where

import Network.HTTP.Client (Manager, httpLbs)
import Data.Text
import Data.Aeson (Value)
import ERPNext.Client (Config, ApiResponse, createRequest, createRequestWithBody, parseGetResponse, parseDeleteResponse)
import ERPNext.Client.Helper (urlEncode)

-- | Get a list of documents for a given DocType name.
-- The filter parameter can contain raw query string parameters.
getDocList :: Manager
           -> Config
           -> Text -- ^ DocType name
           -> Maybe Text -- ^ Optional query string filter
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
