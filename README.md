# erpnext-api-client

This is a Haskell API client for [ERPNext](https://github.com/frappe/erpnext).
It aims to be a light-weight library based on
[http-client](https://hackage.haskell.org/package/http-client) and
user-provided record types.

ERPNext has the concept of
[DocTypes](https://docs.frappe.io/erpnext/user/manual/en/doctype)
which model entities like Customer, Sales Order, etc.

The [ERPNext REST API](https://docs.frappe.io/framework/user/en/api/rest)
basically has seven types of requests covering CRUD operations, remote
method calls, and file uploads.

For CRUD operations on a given DocType there are:

- GET list of all documents (`getDocTypeList`)
- POST to create a new document (`postDocType`)
- GET a document by name (`getDocType`)
- PUT to update a document by name (`putDocType`)
- DELETE a document by name (`deleteDocType`)

Note: Remote method calls and file uploads are not yet supported.

DocTypes in ERPNext can be extended and newly created by users.
This is why this library does not come with any predefined types for DocTypes.
Instead, users can provide their own types with only the fields they need.

This library provides tooling to generate Haskell record types from
a simple DSL to define DocTypes (very similar to
[persistent's model definition syntax](https://hackage.haskell.org/package/persistent/docs/Database-Persist-Quasi.html)).

## Usage

```haskell

import ERPNext.Client
import Data.Aeson

data Customer
  { name :: Text
  }

instance FromJSON Customer
instance ToJSON Customer

instance IsDocType Customer where
  docTypeName = "Customer"

main :: IO ()
main = do
  config = mkConfig "https://erpnext.example.com" "api-key" (mkSecret "api-secret")
  c <- getDocType config "theCustomerId" :: IO Customer
  putStrLn $ name c

```
