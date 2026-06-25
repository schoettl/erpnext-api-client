# Changelog for `erpnext-api-client`

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.3.0.0] - 2026-06-25
### Fixed

- `getDocList`: When the list of requested fields was not exhaustive
  to parse into the specified record type, `getDocList` ended up to
  parse the response into `Ok _ _ []` instead of `Err _ _`. With this
  bug the response looked like we got no results but in reality it was
  just wrong API use.
- Fix parsing bug in transformation script `gen-openapi-yaml.sh`
  related with comments.

### Changed

- Fix names in public API because these functions actually don't
  operate on DocTypes but on documents:
  - `getDocTypeList` -> `getDocList`
  - `getDocType` -> `getDoc`
  - `postDocType` -> `postDoc`
  - `putDocType` -> `putDoc`
  - `deleteDocType` -> `deleteDoc`
- Renamed modules `QueryStringParams` and `Filters` to singular.

### Added

- `getAllFieldnames` to fetch names of all fields for a given DocType.
- `getDocListAllFields` to fetch list of documents with all their
  fields without explicitly naming them.
- `systemFieldnames` to define some fixed fieldnames like `_user_tags` and `_assign`.
- `IsString` instance for `FilterValue` so that `"my value"` is
  automatically converted to `FilterText "my value"`.
- `showJsonPretty`, `showJsonResponsePretty`, and `showApiResponseDebug`
  for debugging and pretty-printing JSON API responses (adds dependency `aeson-pretty`).
- `andThen` and `andThenWith` to concatenate two API calls passing the
  result of A directly to B.
- Module `ERPNext.Client.Simple` for simple versions of the CRUD
  functions to work JSON `Value` objects instead of strict record types.
- Module `ERPNext.Client.Types` for helpers and types dealing with
  DocType status and booleans.
- Helper functions `isNotFound` and `isForbidden` for easy error
  response interpretation.

## [0.2.1.0] - 2026-03-13

### Added

- Added `docName` method to `IsDocType` class to get the document ID.
- Add doc for `getDocTypeList` about default number of rows.

## [0.2.0.0] - 2025-04-12

### Changed

- Refactored the ERPNext.Client.Filters module.

### Fixed

- Fix percent-encoding.
- Fix encoding of double quotes in filters and query string.

## [0.1.0.1]
## [0.1.0.0]

Initial release.
