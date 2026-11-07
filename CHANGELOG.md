# Changelog

All notable changes to Vittix.DBGrid are documented here. Releases older than
v1.0.3 were published before this file existed; their notes live in the GitHub
releases only.

## [2.1.0] - 2026-11-07

### Added

- **Header sort arrows, filter indicators and context menu.** Title cells
  now draw an up/down sort arrow and, for multi-column sorts, the sort
  index. Columns with an active filter show a funnel; hovering a title
  shows a filter button that opens the existing filter popup. Right-clicking
  a title opens a header menu with Sort Ascending/Descending, Clear Sort,
  Filter..., Clear Filter, Freeze up to here/Unfreeze, Choose Columns...,
  Auto-fit Column and Auto-fit All. Double-clicking a title border auto-fits
  the column. New published `ShowSortArrows`, `ShowFilterIndicators` and
  `HeaderMenu` (all default True); with `HeaderMenu = False` the legacy
  right-click filter popup remains. Shift+click adds a multi-sort column
  (Ctrl stays as an alias). Glyphs are DPI-scaled.

### Changed

- **XLSX streaming and row limit.** The worksheet XML is written
  incrementally as UTF-8 to a temp stream and added to the zip from that
  stream instead of being accumulated in a `TStringList`/`TStringBuilder`
  and copied several times. `<cols>` is emitted first using grid-width or
  caption-heuristic widths (no data-driven pass). New options
  `XlsxRowLimit` (default 1,048,576, header/footer included) and
  `XlsxRowLimitAction` (`xraRaise` default / `xraTruncate`) prevent a
  corrupt workbook on oversized exports. Atomic swap, cancellation,
  progress and cursor restore are unchanged.

- **Filter engine compiles per-column conditions.** The classic per-column
  AND path is now compiled once into a condition group and evaluated with
  the same matcher as saved filter sets, removing the per-row re-trim,
  operator parse, Between split and date parse. Direct edits to
  `ColumnInfo.FilterText`/`HasFilter` while active are detected through a
  snapshot and recompiled. Every existing semantic is preserved (NULL vs
  empty, date comparisons, formatted-number parsing, global-search OR).
  Global search now examines visible columns only and skips memo/blob
  fields; the new `GlobalSearchIncludeMemo` option (default False, on the
  controller/engine) opts them back in.

- **Aggregation correctness and scan performance.** Client SUM/AVG/MIN/MAX
  now cover `ftShortint`, `ftByte`, `ftLongWord`, `ftSingle` and
  `ftExtended`, matching the server-side aggregates. `ftBCD`/`ftFMTBcd`
  accumulate in Currency while values and the running sum fit, avoiding
  Double drift (0.1 summed 1,000,000 times is exactly 100,000); larger
  values fall back to the Double path. MIN/MAX work for date/time fields
  and render with the field's `DisplayFormat`. Footer display honors the
  field `DisplayFormat`, and integer SUM no longer shows decimals. The scan
  writes each column's accumulator back once instead of copying the
  record every row, and skips the redundant per-record filter callback
  when the filter engine's OnFilterRecord hook already filters the dataset.

### Added

- **`IncrementalSearchMode` (default `ismAuto`).** Type-ahead search no
  longer consumes printable keys on an editable grid, so typing can start
  in-place editing again. `ismAuto` searches only when the grid cannot edit
  (no `dgEditing`, or the grid/column/dataset is read-only); `ismAlways`
  keeps the pre-2.1 always-search behavior; `ismOff` disables search. The
  existing `IncrementalSearch` Boolean still turns search off when False,
  so old DFMs load unchanged.

### Fixed

- **FireDAC provider keyword scan.** `CheckWrappable` now normalizes
  CR/LF/TAB whitespace and strips string literals, `--` line comments and
  `/* */` block comments before checking for `UNION`, `ORDER BY`, `EXEC`,
  `EXECUTE` and `CALL`. Queries with a multi-line or tab-separated
  `ORDER BY`/`UNION` are now correctly declined instead of being wrapped,
  while keywords that only appear inside literals or comments no longer
  trigger a false fallback. The intentionally over-rejective handling of
  `ORDER BY` (including subqueries) is unchanged.

## [2.0.0] - 2026-10-28

### Added

- **Server-side filter/sort provider (opt-in).** Large datasets can now
  push filtering and sorting into the database as parameterized
  WHERE / ORDER BY instead of evaluating OnFilterRecord per row on the
  client. **Client-side behavior is unchanged and remains the default and
  the correctness baseline: applications that never set the new
  `FilterMode` property see exactly the 1.4 behavior.**
  - New `Vittix.DBGrid.Provider` unit: `TVittixSQLBuilder` (pure,
    dataset-free SQL generation) and `IVittixDataProvider` (the interface
    the controller calls). New published grid property `FilterMode`
    (vcmClient default / vcmServer / vcmAuto — one property governs filter
    AND sort, since a hybrid split would mean two re-queries per change
    and two disagreeing row orders). New controller `Provider` property
    accepts application-supplied providers; events
    `OnProviderFallback(Sender, Reason)` and
    `OnProviderError(Sender, E, var Handled)` surface declines and failing
    queries.
  - SQL generation per dialect (ANSI, Firebird, SQLite, MySQL/MariaDB,
    PostgreSQL, SQL Server): quoted and validated identifiers (unknown
    field names are rejected, never passed through), UPPER-LIKE
    case-insensitive matching with `ESCAPE`, `NULL`/blank/Between/comparison
    operators, typed date/number/boolean parameters (user values never
    appear in the SQL text — injection payloads stay in parameters by
    construction), explicit `NULLS FIRST/LAST` on PostgreSQL/Firebird and
    documented defaults elsewhere, AND-within/OR-between group semantics
    with parenthesization, and a capped global search OR across text
    columns (20-column limit) that keeps its "replaces column filters"
    semantics. Operators that depend on client display text (formatted
    numbers, OnValidateFilter hooks) are reported "not expressible" so
    vcmAuto falls back.
  - New `Vittix.DBGrid.Provider.FireDAC` unit (separate
    `VittixDBGridControllerFD` runtime package so the core does not
    hard-depend on FireDAC): wraps the application's TFDQuery SQL as
    `SELECT * FROM (original) VITTIX_SUB WHERE ... ORDER BY ...`, restores
    the original SQL and parameter values on clear/mode switch/controller
    teardown and after a failing query, preserves the current record
    best-effort by field values, and computes footer aggregates with one
    `SELECT COUNT/SUM/AVG/MIN/MAX` against the same WHERE (client scan
    remains the fallback). Unwrappable shapes (UNION, top-level ORDER BY,
    semicolons, EXEC/CALL commands) are declined with a reason.
  - Integration: the filter popup, global search, saved filter sets,
    layout restore and header-click sort all route through the provider
    when a server mode is active; edits defer the re-query like the
    aggregation deferral; a failing query restores the previous state and
    fires `OnProviderError` (re-raises when unhandled); the footer shows
    "Server filter (dialect)" / "Client filter (fallback)" in the hint
    area; exports see the already-filtered dataset so `ExportFilteredOnly`
    is a no-op there; incremental search uses Locate on the loaded rows
    (type-ahead covers fetched rows only when FetchOptions limits the
    fetch).

- **Repository hygiene.** UTF-8 BOMs added to 11 sources containing
  non-ASCII text (previously compiled as ANSI mojibake); `*.dres`/`*.vrc`
  build artifacts ignored; one mixed-line-ending file normalized.

- **Demo.** A Data Mode combo (Client/Server/Auto) over an in-memory
  SQLite table of 100,000 rows with a live status label showing mode
  switch time.

### Changed (behavior)

- **`SetGlobalFilter` re-applies immediately in server modes too** (the
  1.4 fix covered client mode; the routing is shared now).
- **The client sort engine defers to the server** while a server sort is
  active: `IndexFieldNames` is cleared before the first server ORDER BY so
  it cannot re-sort the re-opened result client-side, and restored on
  fallback.
- **The controller unregisters its dataset FreeNotification on
  destruction**, so a dataset outliving its grid-owned controller no
  longer notifies a dead object (found by the new teardown tests).

### Removed

- **`TVittixDBGridColumnInfo.EditMask`** (introduced in 1.4.0, never
  consumed by any editor) is gone from the column info, the layout JSON
  writer and the docs. Layout files that carry an `editMask` key still
  load — unknown keys are ignored by the reader. `TVittixEditorKind` and
  the masked numeric editor itself are unaffected (they are driven by
  Min/Max/DecimalPlaces).

### Fixed

- **`SetGlobalFilter` while a filter set's OR-groups were active** did not
  re-route the conditions (server mode).
- **An empty OR-group ORed the whole filter away** server-side too
  (compile-time drop, matching the 1.4 client fix).
- **Sort-state restore no longer wipes an application's own
  `IndexFieldNames`** when no client sort had been applied.

### Tests

- 42 new tests: 29 dataset-free SQL-builder tests across all dialects
  (every operator, groups/parentheses, global search cap, typed
  parameters, ORDER BY NULL placement, LIKE escaping, injection payloads,
  unknown fields, not-expressible reasons) and 13 FireDAC/SQLite
  integration tests (client/server result equivalence, OR-groups, global
  search, multi-column sort, SQL restore on clear/destroy, vcmAuto
  fallback with reason, failing-query restore + error event,
  record preservation by key, server aggregates vs client aggregates,
  saved filter set and layout round trips in server mode, edit-state
  deferral, `ExportFilteredOnly` in server mode, and a 200k-row timing
  comparison) — 382 total.

## [1.4.0] - 2026-10-27

### Added

- **Saved filter sets with OR-groups.** The filter engine gains a group
  model: a set is an ordered list of groups, conditions inside a group are
  ANDed and groups are ORed (empty groups are ignored). Conditions reuse
  the existing operator-prefix syntax and the single operator table; each
  group is precompiled at apply time (parsed operator, folded needle,
  typed numeric/date values), so per-record evaluation no longer parses.
  The classic per-column AND behavior is exactly one implicit group and is
  unchanged.
- **New `Vittix.DBGrid.FilterSets` unit** with `TVittixFilterSet`,
  `TVittixFilterSetGroup`, and `TVittixFilterSetList` (add/replace, delete,
  rename, move up/down, find by name) with versioned JSON persistence
  written atomically (staged temp file, missing directories created). A
  corrupt file raises `EVittixFilterSetError` on every access until it is
  fixed — saved data is never silently dropped or overwritten.
- **Controller API:** `SaveCurrentFilterAs`, `ApplyFilterSet`,
  `DeleteFilterSet`, `FilterSetNames`, `FilterSetList`, `PersistFilterSets`,
  `ActiveFilterSetName` plus the new published grid property
  `FilterSetsFileName` (per-grid instance setting; empty resolves to
  `PersistenceRootPath\filtersets.json`). New events `OnFilterSetApplied`
  and `OnFilterSetsChanged` (controller and grid share storage).
- **Layout integration:** the layout JSON stores `activeFilterSet`
  (optional v2 field, no schema bump). Applying a layout re-applies the set
  when it exists and skips unknown names silently.
- **Saved-filters UI:** the filter popup gains a `Saved...` menu (apply a
  named set, save the current filters as..., open the manager) and a new
  modal `TVittixDBGridFilterSetManager` dialog (list, rename, delete,
  reorder, and a group/condition editor driven by the operator table).
  Both are style-aware. While a set is active its name shows in the
  footer's hint area; `ClearFilters` clears it.
- **Richer in-place editors (per-column).** New `TVittixDBGridColumnInfo`
  options: `EditorKind` (veNone default, vePickList, veLookup,
  veMaskedNumber), `PickItems` ("value=caption" pairs write the value to
  integer fields), `EditMask`, `HasMin`/`MinValue`, `HasMax`/`MaxValue`,
  `Required` and `DecimalPlaces`. The pick list, lookup (searchable list
  from the field's `LookupDataSet`, single key field) and masked numeric
  editors are modal popups opened by F2/double click; read-only columns
  and fields open nothing, and Esc cancels the edit cleanly. All options
  round-trip through the layout (optional v2 fields).
- **`OnValidateCell` validation** (controller, re-published on the grid):
  runs before any Vittix editor commits (memo, date, pick list, lookup,
  masked numeric) after the built-in Required/Min/Max rules; on
  `Accept = False` the message is shown and the editor stays open. The
  event receives `var NewValue` and may rewrite it.

### Changed (behavior)

- **The global search now re-applies immediately.** `SetGlobalFilter` while
  the filter engine was already active silently did nothing before (the
  `Active := True` assignment was a no-op), so typing a new global search
  kept the previous result. It re-filters now. Flagged as a bug fix below.
- **Non-ASCII type-ahead search takes the fast Locate path on wide string
  fields.** `ftWideString`/`ftFixedWideChar`/`ftWideMemo` fields compare in
  UTF-16 (Midas keeps strings wide; FireDAC routes dtWideString through
  `CompareStringW` with `NORM_IGNORECASE`), so those searches use
  `Locate` again; ANSI `ftString` fields keep the Unicode scan fallback.
- **The fallback scan avoids a per-row string allocation.** The search
  needle is folded once and rows are compared char-wise: a full 50,000-row
  Unicode prefix sweep measures ~21 ms.
- The filter popup is slightly wider to fit the Saved button; new editors
  and dialogs follow the active VCL style.

### Fixed

- **A non-ASCII type-ahead search could locate the wrong record on ANSI
  fields** (found in 1.2; the Locate fast path is now gated to ASCII
  buffers and wide fields).
- **`SetGlobalFilter` was a no-op while the engine was already active**
  (see above).
- **An empty OR-group would vacuously match every record** and OR the
  whole filter away; empty groups are dropped at compile time.
- **A corrupt filter-set file was loaded as an empty list on the second
  access**; the list now re-raises on every access and can never overwrite
  the file until it parses.

### Tests

- 31 new regression tests: wide-string fast path (with Locate-call
  counting), 50k-row scan performance, group semantics/equivalence, filter
  set save/apply/persistence/corruption/isolation, layout integration,
  editor helpers, validation gate ordering, read-only and lookup safety,
  and editor-option layout round trips — 340 total.

## [1.3.0] - 2026-10-27

### Added

- **Column freezing (fixed left columns).** New published property
  `FrozenColumns` on the grid (default 0; the setter clamps to
  0..VisibleColumnCount-1) keeps the leftmost visible columns fixed while
  the rest scroll horizontally, plus `FreezeUpToColumn(Column)`,
  `UnfreezeAll` and `RefreshFrozenColumns` and the
  `OnFrozenColumnsChanged(Sender)` event. Built on `TCustomGrid.FixedCols`
  (re-applied after every layout change, since TDBGrid resets it); frozen
  and scrolling regions cannot be crossed by VCL column drags (the move is
  rolled back) or by column-chooser drags/keyboard moves. The footer keeps
  aggregates aligned under frozen columns while scrolled. The per-column
  `Pinned` flag persists in the layout; `ResetLayout` and `UnfreezeAll`
  clear freezing, and hiding a frozen column in the chooser shrinks the
  region.

- **Layout schema v2.** The layout JSON gains a `version` field (always
  written as 2; v1 files without it load unchanged with their previous
  behavior) and per-column `pinned`, `titleCaption`, `displayFormat`,
  `exportInclude` and `exportCaption`. Unknown fields from future versions
  are ignored; malformed input still raises `EVittixLayoutError`. Matching
  new properties on `TVittixDBGridColumnInfo` (all defaulting to previous
  behavior) with `Assign` support; `ApplyLayout` pushes `TitleCaption` to
  the grid column and `DisplayFormat` to the backing numeric/datetime field
  (empty strings leave the current values untouched).

- **Export column settings.** `ExportInclude = False` excludes a column
  from every export format and the clipboard copy (even when
  `ExportVisibleOnly` is off); a non-empty `ExportCaption` replaces the
  header caption in CSV, TSV, HTML, Text, XLSX and copy headers (XML/JSON
  keep their field-name keys).

- **Unicode-correct incremental search.** Type-ahead matching now folds
  case through Unicode simple case mapping (`TCharHelper.ToUpper`):
  accented Latin, Cyrillic and Greek match case-insensitively, and the
  caseless Indic scripts (Devanagari, Gujarati) match positionally without
  corruption.

- **Surrogate-pair-safe search buffer.** Characters outside the BMP enter
  the buffer as a pair; the search runs once the low surrogate completes
  the pair, and Backspace removes both halves together (a lone surrogate
  never survives). Combining marks (Indic matras, virama, nukta) delete one
  UTF-16 code unit per Backspace, matching the Windows EDIT control's
  granularity. Characters injected per `WM_CHAR` by keyboard hooks search
  as before.

- **Clipboard shortcut bypass during edits.** While the in-place editor is
  open (or a dataset edit/insert is pending), Ctrl+C/X/V/A are no longer
  consumed by the grid copy; the editor's native text handling wins. The
  decision is isolated in
  `TVittixDBGridController.ShouldBypassClipboardShortcut`.

- **Demo.** The feature demo gained a VCL style switcher (Carbon,
  Windows10 Dark, Windows11 Modern Dark embedded via project resources,
  with a `styles\` folder fallback and a graceful no-styles path), an
  incremental-search status label, a one-click XLSX export writing typed
  numbers/dates and NULL cells (every 5th row has null Notes/Shipping
  Date), and multi-select enabled for the copy/selection-aggregate tour.

### Changed (behavior)

- **Layout files are written as schema v2.** Old versions reading v2 files
  ignore the new keys; this version reads v1 files unchanged.
- **Ctrl+C during an edit goes to the editor.** With an in-place editor
  open, Ctrl+C/X/V/A reach the editor instead of the grid copy.
- **Column chooser and footer surface small public helpers**
  (`SetColumnChecked`, `ApplySelection`, `GetColumnRect`,
  `GetIndicatorRect`) used by the new features and tests.

### Fixed

- **A non-ASCII type-ahead search could locate the wrong record.**
  `TClientDataSet.Locate` compares `ftString` values after a codepage
  conversion that collapses every non-ASCII character to `?`, so a
  Devanagari buffer matched a Gujarati row. Non-ASCII buffers now take the
  Unicode-correct fallback scan; ASCII buffers keep the fast Locate path.

### Tests

- 35 new regression tests (Unicode/surrogate search, editor bypass, layout
  schema v1/v2 round trips, export column settings, frozen columns
  including drag rules, layout persistence, footer alignment and chooser
  interaction) — 309 total.

## [1.2.0] - 2026-10-27

### Added

- **Incremental search (type-ahead locate).** Printable characters build a
  search buffer and the grid locates the first record whose primary sort
  column (or current column, when unsorted) starts with the buffer,
  case-insensitive. The buffer resets after `IncrementalSearchTimeout`
  milliseconds (default 1000) or on Esc; Backspace removes the last
  character. In-place editing (open editor or edit/insert state) bypasses
  the search entirely. Datasets without `Locate` support fall back to a
  cursor-restoring scan. New published properties on the grid:
  `IncrementalSearch` (default True), `IncrementalSearchTimeout`
  (default 1000) and the `OnIncrementalSearch(Sender, SearchText, Found)`
  event (shared storage with the controller, like the other notification
  events). The current buffer shows as a hint in the footer's left corner
  and clears on reset.

- **Copy selection to the clipboard.** Ctrl+C copies the current cell, or
  the selected rows when `dgMultiSelect` holds more than one row, as TSV.
  The new grid property `CopyIncludeHeaders` (default False) prepends a
  header row of the visible column captions; Ctrl+Shift+C always includes
  headers. Values use the export engine's full-precision invariant
  formatting (`ExportLocaleFormat` switches to display text), respect the
  visible columns and their order, quote values containing tabs, quotes or
  line breaks, write NULLs as empty fields, and preserve the dataset
  cursor. The public `TVittixDBGridExporter.BuildCopyText(AIncludeHeaders)`
  builds the same text without touching the clipboard.

- **Selection aggregates in the footer.** While a multi-row selection is
  active, each footer column shows the aggregate of the selected rows only
  (`Sum: x` for numeric fields, `Count: n` otherwise) without changing the
  dataset-wide aggregation state; clearing the selection restores the
  configured footer.

- **VCL Styles support.** New `Vittix.DBGrid.Styles` unit with
  `TVittixStylePalette`, a style-aware color resolver. The footer panel's
  background, text and 3D borders, the filter popup's button panel and
  validation label, and the export dialog's banner follow the active VCL
  style (per-control style services). When no custom style is active the
  resolvers return the live system colors, so unstyled rendering is
  unchanged.

- **XLSX column widths.** The worksheet gains a `<cols>` element derived
  from the grid column widths (a caption-length heuristic per column where
  no usable grid width exists).

### Changed (behavior)

- **XLSX dates are real dates now.** Date, time and timestamp fields are
  written as Excel serial numbers (`t="n"`) with a date/time/datetime
  number format from the package's new `xl/styles.xml`, instead of display
  text. Excel can sort and compute on them.
- **XLSX NULLs are empty cells.** A NULL exports as a cell with no value
  instead of an empty inline string, so Excel's per-column type inference
  stays clean.
- **XLSX honors `ExportLocaleFormat`.** With the opt-out on, every cell is
  now written as locale display text (the pre-1.2 wholesale-text behavior);
  previously the XLSX writer ignored the option and always emitted
  invariant numbers, contradicting the documented contract.
- Numeric XLSX cells carry an explicit `t="n"` attribute, and integer-family
  fields are written from their exact digits (an Int64 beyond 2^53 keeps
  its precision; a Double round trip would round).

### Fixed

- `vefPDF` is documented as not implemented and hidden from the export
  dialog (the enum member is kept so existing compiled code keeps loading);
  `ExportToStream(vefPDF)` still raises `EVittixExportError`.

### Tests

- 43 new regression tests covering the typed XLSX cells (null, negative,
  currency, date/datetime/time, large integers, styles part, column
  widths, whitespace preservation, locale opt-out), the style palette,
  incremental search (match, no-match, backspace, timeout, sorted vs
  unsorted column, edit-mode bypass, filtered and closed datasets, the
  no-Locate fallback) and the selection copy (single cell, multiselect,
  headers, hidden columns, nulls, tabs/quotes/newlines, cursor
  preservation, footer aggregates) — 274 total.

## [1.1.1] - 2026-10-05

### Fixed

- Date/time filters compare typed values for equality, inequality, ordered
  comparisons and ranges using locale-aware parsing; null dates are excluded
  from comparable values. Unparseable text keeps existing display matching.

- Exports reject reentry on the same exporter and restore the grid's prior
  Enabled value after success, cancellation or an exception.

- Unhooking the controller preserves a subsequently installed WindowProc.

- Removed the unused fixed-row message and its message-swallowing branch.

- ResetLayout restores field order, measured widths, visibility, footer
  visibility, and clears aggregations, footer text and cell conditions.

- Saving layouts creates missing persistence directories before staging files.

- Clearing filters preserves an OnFilterRecord handler installed later by
  the application.

- Cell drawing skips field-to-string conversion when no cell conditions exist.

- Footer aggregation changes defer scans during edits/inserts, preserve the
  pending refresh, and no longer implicitly post records.

- Right-clicking the first data row with titles hidden no longer opens a
  header filter popup or column chooser.

- Layout loading now refreshes aggregate values and footer geometry after
  applying batched column changes.

### Changed

- Documented the deferred Count/Sum incremental aggregation design and its
  correctness prerequisites in the roadmap; full recalculation remains active.

- Export message pumping is throttled to 50 ms intervals; the dialog no
  longer performs a duplicate pump.

- Added a dataset-owner teardown guard with an active filter; freeing the
  dataset before the grid did not reproduce a dangling-pointer crash.

- Added a regression guard for aggregate refresh after Delete, Insert/Post,
  Edit/Post and Cancel; the reported stale totals did not reproduce.

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
