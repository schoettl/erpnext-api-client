# Changelog for `erpnext-api-client`

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.3.0.0] - 

### Changed

- Fix names in public API because these functions don't operate on
  DocTypes but on documents:
  - `getDocTypeList` -> `getDocList`
  - `getDocType` -> `getDoc`
  - `postDocType` -> `postDoc`
  - `putDocType` -> `putDoc`
  - `deleteDocType` -> `deleteDoc`

### Added

- `getDocListAllFields` to fetch list of documents with all their
  fields without explicitly naming them.

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
