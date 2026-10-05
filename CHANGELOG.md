# Changelog

## [1.1.1] - 2026-10-05

### Fixed

- Layout loading now refreshes aggregate values and footer geometry after
  applying batched column changes.

### Changed

- Added a regression guard for aggregate refresh after Delete, Insert/Post,
  Edit/Post and Cancel; the reported stale totals did not reproduce.

All notable changes to Vittix.DBGrid are documented here. Releases older than
v1.0.3 were published before this file existed; their notes live in the GitHub
releases only.

## [1.1.0] — 2026-09-29

### Added

- `ExportLocaleFormat` export option: when True, CSV/TSV/XML/JSON/XLSX fall
  back to the locale-aware display formatting (`FloatFormat`,
  `CurrencyFormat`) instead of the default full-precision invariant output —
  the opt-out for consumers who open exports in locale-sensitive tools such
  as a comma-decimal Excel.
- `OnSortError` event on the controller (re-published on the grid): fired
  when an interactive header-click sort fails; the sort state is rolled back
  first. Without a handler the underlying `EVittixSortError` is re-raised.
- `Vittix.DBGrid.Clipboard` unit with retry-tolerant clipboard write/read
  helpers, used by exports, footer copy actions and the tests. Clipboard
  managers, RDP sessions and monitoring tools that briefly hold the
  clipboard open no longer make a copy/export fail with
  "Cannot open clipboard: Access is denied".

### Changed (behavior)

- **CSV, TSV, XML and JSON numbers look different.** They are written with
  full precision and an invariant decimal separator (`1234.5678`) instead of
  the locale-aware `0.00` display format (`1234.57`). Set
  `ExportLocaleFormat := True` to keep the old output.
- **JSON gains a `__footer__` object** as the last array element when
  `IncludeFooter` is enabled, marked so consumers never mistake it for a
  record.
- **Sorting an unsupported dataset raises.** Applying a sort to a dataset
  without `IndexFieldNames` (for example `TADODataSet`) raises
  `EVittixSortError` instead of silently doing nothing. Header clicks on
  such a dataset roll the sort state back and raise — or report through
  `OnSortError` when a handler is assigned.

### Fixed

- **CSV/TSV/clipboard export corrupted numbers.** Formula-injection
  neutralization applied to every cell turned `-5`, `-12.50` and `+44...`
  phone numbers into `'-5` text cells. Values that parse as numbers are now
  exported untouched; real formula text (`=SUM(1,2)`, `+SUM(1,2)`) is still
  neutralized.
- **A cancelled export broke the next one.** The cancel flag was only reset
  by `ExportToStream`, so after one cancellation every file-based export
  (`ExportToCSV`, `ExportToHTML`, `ExportToXML`, `ExportToJSON`,
  `ExportToExcel`) aborted at the first row. All entry points reset the flag
  now.
- **XML, JSON and Text exports were second-class.** They ignored
  `ExportFilteredOnly = False`, ignored `IncludeFooter`, and left the dataset
  cursor at EOF so the grid jumped to the last record. All three now share
  the CSV/HTML/XLSX iteration helpers and footer support (JSON marks its
  footer object with a `"__footer__"` key so it is never mistaken for a
  record).
- **JSON export was not real JSON.** Numbers were written as `"42"`, nulls as
  `""`, booleans as `"Yes"/"No"`, and control characters below 0x20 were
  emitted raw. JSON now writes native numbers, `true`/`false`, `null`, and
  `\u00XX`-escapes all control characters; the output parses as JSON.
- **The `<>` filter operator was really "does not contain".** `!` and `<>`
  both mapped to a substring-exclusion match. `<>` is now a true Not Equals
  (exact, case-insensitive value comparison) while `!` keeps the Does Not
  Contain behavior. Operator table order is unchanged, so persisted operator
  indexes remain valid.
- **Clearing the grid filter forgot the dataset's own filter state.** The
  engine now saves the dataset's `Filtered` value when its hook is installed
  and restores it on clear, so an application's own `Filter` /
  `OnFilterRecord` filtering survives.
- **Export/layout file replacement could destroy the old file.**
  `ExportToFileAtomic` used delete-then-move; it now uses `TFile.Replace`
  (with a managed backup file) so a locked target leaves the original
  intact. Layout `SaveLayoutToFile` gained the same staged temp-file +
  replace treatment, so a crash mid-write can no longer corrupt a saved
  layout.
- **XLSX stored every value as text.** Numeric fields are now numeric cells
  (`<v>100.5</v>`), booleans are `t="b"` cells, and inline strings carry
  `xml:space="preserve"`.
- **XML export could produce files Excel rejected.** Control characters
  illegal in XML 1.0 (0x00–0x08, 0x0B, 0x0C, 0x0E–0x1F) are now dropped, and
  tag names are fully sanitized (any invalid character, not just space, `-`
  and `.`).
- **Text export split records across lines.** Embedded line breaks in field
  values broke the fixed-width layout; they are flattened to spaces now.
- **Lifetime safety.** The controller and the exporter now register
  `FreeNotification` for the grid and dataset they reference, so a grid or
  dataset owned by a data module can be freed before them without leaving
  dangling pointers.
- **Applying or clearing a filter did not re-evaluate an already-filtered
  dataset.** When the application was filtering before the engine hook
  installed (or after it was removed), assigning the same `Filtered` value
  was a no-op, so records were never re-evaluated against the combined
  filter and stale rows stayed visible.
- **Footer sync was heavy.** The footer panel caches its text metrics and
  only invalidates when its geometry actually changed; aggregation updates
  invalidate the footer explicitly.

### Documentation

- Export header comment no longer advertises XLS/OLE/FlexCel/PDF support
  that does not exist; the demo no longer requires the commercial madExcept
  suite (roadmap 6.3).
- `tests/build-tests.bat` accepts a `VITTIX_DELPHI_ROOT` override instead of
  only the hardcoded Delphi 12 path.

### Repository

- CONTRIBUTING.md corrected (the grid does subclass `TDBGrid`, real folder
  layout, real demo name, supported Delphi versions).
- README.md now documents export formats, filter operator syntax, layout
  persistence and the test suite; machine-dependent claims were toned down.
- `CHANGELOG.md` added (this file); `release-notes-v1.0.3.md` moved here.
- Machine-specific entries removed from `VittixDBGridControllerR.dproj`
  (`Excluded_Packages` pointing at a Studio 21.0 path, `DeployFile` pointing
  at a user-local Bpl directory); package versions aligned at 1.1.0.0 and
  the installer version at 1.1.0.
- `.gitignore` no longer blanket-ignores `*.res` (the committed project
  `.res` files are intentional); empty `docs/README.md` became a docs index;
  stale `source/Vittix.DBGrid.pas.bak` and 0-byte `source/VittixDBGrid.res`
  removed; changelog-style noise comments cleaned from source.

### Tests

- 31 new regression tests covering all of the above (213 total).

## [1.0.3] — Footer Alignment, Tests, and Installer Automation

### Added

- DUnitX test suite covering core grid behaviors
- Inno Setup installer for component distribution
- Build automation for installer payload generation
- GitHub release automation for tagging and publishing

### Changed

- Footer summary layout now syncs with DBGrid column geometry
- Startup footer alignment now uses actual grid cell rectangles
- Installer packaging now supports Delphi 12 Athens source and package deployment

### Improved

- Footer summary behavior when columns are resized
- Footer summary behavior when columns are shown or hidden
- Release workflow for publishing installer-based component builds

### Fixed

- Footer summary cells not lining up with DBGrid columns at startup
- Footer alignment drift caused by coordinate-space mismatch
- ClientDataSet sorting support and related sorting behavior regressions
