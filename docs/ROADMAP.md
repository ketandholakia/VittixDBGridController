# VittixDBGridController — Fix & Improvement Roadmap

This roadmap is based on a full code audit (August 2026). Items are grouped into
milestones ordered by effort-to-impact ratio. Each item lists the concrete files
and locations involved so it can be picked up as a self-contained task.

Legend: **[B]** bug fix · **[R]** refactor · **[P]** performance · **[F]** feature · **[D]** docs/hygiene

---

## Milestone 1 — Correctness fixes (small effort, high impact) — ✅ COMPLETED 2026-08-16

All items below are implemented and covered by the test suite (130 passing,
0 failing, 3 ignored with documented reasons). Additional fixes discovered
while repairing the previously-unrun test suite:

- Removed the popup's in-memory operator suggestion entirely (its owner-class
  keyed global state leaked operators between grids/sessions); operator memory
  now comes only from the per-grid INI file.
- `ApplyChanges` now persists the filter history INI on every commit, not only
  when the filter text changed.
- Consolidated popup operator parsing into `ApplySavedTextToControls`, used by
  both the active-filter path and persisted-history restore (also fixes the
  missing `!..` Not-Between prefix in the popup and keeps raw prefixes out of
  the value combo).
- Export dialog: `LoadDialogState` restored format index 6 to the Text radio;
  the correct ordinal is 8 (`vefText`).
- `ApplyLayout` pushes footer state through both the grid and controller
  setters so a direct `ShowFooter` assignment cannot desync them.
- Controller layout save/load falls back to `PersistenceRootPath\layout.json`
  when no explicit file is configured.

### 1.1 [B] Remove global static persistence settings
Per-grid settings (`PersistenceRootPath`, chooser/filter-history file names) are
pushed to unit-global class variables shared by ALL grids in the process, so two
grids on one form corrupt each other's state.

- `source/Vittix.DBGrid.pas:306-312` (`ApplyPersistenceSettings` writes to
  `TVittixDBGridColumnChooserForm.RootPath`, `TVittixDBGridFilterPopup.RootPath`, etc.)
- Fix: make these instance fields on the controller, passed as parameters to
  `TVittixDBGridColumnChooserForm.Execute(...)` / `TVittixDBGridFilterPopup.Execute(...)`
  instead of class properties.

### 1.2 [B] Invalidate the filter engine field cache on dataset deactivation
`FFieldCache` holds raw `TField` pointers. If the dataset is closed/reopened
after `ApplyFilter` without a re-apply, `DoFilterRecord` dereferences dangling
pointers → AV.

- `source/Vittix.DBGrid.Filter.Engine.pas:150-172` (`RebuildFieldCache`)
- Fix: clear the cache in `DataLinkActiveChanged`/`DataSetChanged` paths
  (`Vittix.DBGrid.Controller.pas:492-523`), and re-check field validity in
  `AcceptCurrentRecord`.

### 1.3 [B] Nil-guard engine Clear/summary methods
These iterate `FColumns` without checking it is assigned and will AV when an
engine is constructed without a columns collection:

- `source/Vittix.DBGrid.Filter.Engine.pas:260-271` (`Clear`)
- `source/Vittix.DBGrid.Sort.Engine.pas:340-344` (`ClearSorting`), `:401` (`GetSortSummaryText`)
- `source/Vittix.DBGrid.Aggregation.Engine.pas:376` (`GetActiveAggregationSummaryText`)

Fix: add `if not Assigned(FColumns) then Exit;` (or `Exit('')`) guards.

### 1.4 [B] Harden `TVittixDBGridLayoutJsonStorage.LoadFromStream`
- The `as TJSONObject` cast sits outside the `try`, so a non-object JSON root
  (e.g. an array) leaks the already-created result state.
- A nil root silently returns an empty state — indistinguishable from a valid
  empty layout.
- `source/Vittix.DBGrid.Layout.pas:171-214`
- Fix: move parsing inside the `try`; raise a dedicated
  `EVittixLayoutError` on malformed input.

### 1.5 [B] Give `SaveToFile('')` a real fallback or drop the default parameter
`TVittixDBGridLayoutJsonStorage.SaveToFile('')` and the controller's
`SaveLayoutToFile('')` silently do nothing, which the default parameter makes
look like a supported call.

- `source/Vittix.DBGrid.Layout.pas:223-226`, `source/Vittix.DBGrid.Controller.pas:1047-1058`
- Fix: fall back to the controller's `FLayoutStorageFileName`, raise when no
  target can be determined.

### 1.6 [B] Clean up temp file on failed atomic export — ✅ COMPLETED 2026-09-29
`ExportToFileAtomic` stages a unique temp file next to the target, deletes it
on failure/cancellation, and swaps the result over the target with
`TFile.Replace` (with a managed backup name — the RTL raises on an empty
backup path) so a locked target never leaves the old file destroyed.

### 1.7 [B] Fix column chooser drag index desync
The checklist item is moved first, then the grid column is moved using the
stale pre-move index. Correct only while list and grid stay in sync.

- `source/Vittix.DBGrid.ColumnChooser.pas:581-585` (`CheckListDragDrop`), `:703-704` (`MoveSelectedItem`)
- Fix: resolve the `TColumn` object BEFORE moving the list item, then assign
  `Col.Index`.

### 1.8 [B] Filter engine display-text quirks
- Formatted numbers (thousand separators) fail `TryStrToFloat` and the record is
  rejected instead of passed: `Filter.Engine.pas:449-459`.
- `vfmIsNull` conflates NULL and blank text: `:461-462`.
- `vfmEndsWith` StartPos clamping false-positives on short haystacks: `:398-401`.
- `ParseFilterMode` prefix matches are not length-delimited (`nullity` parses as
  `null`): `:352-375`.
- Fix: match against `Field.Value`/`IsNull` where possible instead of
  `DisplayText`; validate prefix consumption; add unit tests for each case.

---

## Milestone 2 — Structural refactor (medium effort, removes a whole bug class)

### 2.1 [R] Replace event-handler hooking with virtual overrides — ✅ COMPLETED 2026-08-18
The controller saves/replaces `OnTitleClick`, `OnDrawColumnCell`, `OnMouseDown`,
`OnDblClick`, `OnKeyDown` and the `WindowProc` on the grid. Any later assignment
to those properties by application code silently unhooks sorting/colors, and
stacked hooks are a recurring source of ordering bugs.

- Implemented: `TVittixDBGrid` now overrides `TitleClick`, `DrawColumnCell`,
  `MouseDown`, `DblClick`, `KeyDown` and calls new public controller methods
  (`DoTitleClick`, `DoDrawColumnCell`, `DoMouseDown`, `DoDblClick`,
  `DoKeyDown`). The mouse/dblclick/keydown methods return `True` when the
  controller consumed the event (filter popup, column chooser, memo editor, F2).
- `DrawColumnCell`: controller tweaks Brush/Font first, then the application's
  `OnDrawColumnCell` fires via inherited, else `DefaultDrawColumnCell` runs —
  matching the previous hook behavior.
- All five `FOldXxx` event fields and the save/restore in `HookGrid`/`UnhookGrid`
  are gone. Only the `WindowProc` hook remains (footer sync messages — no VCL
  virtual exists for those).
- Application event assignment after startup can no longer unhook the
  integration — covered by new regression tests
  (`ApplicationTitleClickHandlerFiresAndSortingStillWorks`,
  `ApplicationKeyDownHandlerStillFires`, `ApplicationDblClickHandlerStillFires`).
- New public events (`OnAfterSort`, `OnFilterApplied`, `OnAfterApplyLayout`)
  deferred to the feature backlog.

### 2.2 [R] Eliminate the protected-member crackers — ✅ COMPLETED 2026-08-18
`TVittixGridHelper = class(TCustomDBGrid)` and `TVittixGridAccess = class(TDBGrid)`
reached protected members from outside. Since `TVittixDBGrid` is our own
descendant, promote the needed members (`LeftCol`, `CellRect`,
`IndicatorOffset`) as public utilities on `TVittixDBGrid` instead.

- Implemented: `TVittixDBGrid.GetLeftCol`, `GetCellRect`, `GetIndicatorOffset`
  are public; both grid cracker classes are deleted. `TVittixCDSAccess` in the
  sort engine stays — `IndexName` is protected on `TCustomClientDataSet`, a
  framework class we don't own, so cracking there remains the correct pattern.

### 2.3 [R] Single source of truth for visual properties — ✅ COMPLETED 2026-08-18
The grid keeps local copies of `FooterVisible`/`AlternatingRowColors`/
`AlternateRowColor` AND pushes them to the controller. Replace the duplicated
fields with direct delegation to the controller (keep published property
signatures unchanged for DFM compatibility).

- Implemented: the grid's local `FFooterVisible`/`FAlternatingRowColors`/
  `FAlternateRowColor` fields are gone. Published properties delegate to the
  controller (with static-default fallbacks when the controller is absent
  during early teardown). `ApplyLayout` pushes through the grid setters only —
  the old dual-setter workaround is removed since desync is no longer possible.

### 2.4 [R] Delete dead scaffolding — ✅ COMPLETED 2026-08-16
- Empty `TraceGrid`/`TraceController`/`TraceFooter` stubs called ~45 times
  (`Vittix.DBGrid.pas`, `Controller.pas`, `FooterPanel.pas`).
- Commented-out `DebugMsg` (`Controller.pas`).
- Duplicated/dead `FLineBreak` option (`Export.Engine.pas`).
- Dead `Lines.Add` before overwrite in preview (`Export.Dialog.pas`)
  and the `GetPreviewText` placeholder it fed.
- Public raw field `Aggregation: TVittixAggregation` → property
  (`ColumnInfo.pas`). Record member mutations through the property shuttle
  through locals in the aggregation engine (`Inc`/`Clear` would otherwise
  operate on compiler temporaries).
- Redundant RTTI `SetPropValue(..., 'IndexName', ...)` paths
  (`Sort.Engine.pas`) → direct access via a local
  `TVittixCDSAccess = class(TCustomClientDataSet)` cracker (`IndexName` is
  protected on `TCustomClientDataSet`).

### 2.5 [R] Exception hygiene — ✅ COMPLETED 2026-08-16
- Sort engine bare `except end` blocks now catch `EDatabaseError` only
  (temp-index deletion paths).
- Filter popup INI write failure logs via `OutputDebugString` in DEBUG
  builds instead of a silent swallow.
- Dedicated `EVittixExportError` for all export engine raises (PDF stub,
  unsupported format, inactive dataset).

---

## Milestone 3 — Performance & scalability

### 3.1 [P] Incremental aggregation

**Design reviewed 2026-10-05; implementation deferred (request item 14).**
The aggregation engine now exposes `FullScanCount` (2026-11-04) to measure
actual dataset traversals and establish the baseline for a future committed-
mutation adapter. No delta path is enabled without that adapter.
The item 1 TClientDataSet guard is green. Full recalculation remains the
correctness baseline; see the design note below before implementing deltas.
`DataLinkRecordChanged` marks aggregation dirty on every record change, and
refresh triggers a full dataset scan. For Count/Sum/Min/Max it is cheap to
update accumulators incrementally on `Field.NewValue`/post/delete when only one
record changed; fall back to full recalculation on filter/sort changes.

- `source/Vittix.DBGrid.Controller.pas:525-539, 805-828`; `Aggregation.Engine.pas:250-328`

### 3.2 [P] Precompute uppercase in filter matching — ✅ IMPLEMENTED 2026-11-04
The classic column path now caches each uppercased needle when filters are
applied, refreshing it after direct metadata edits. Global search text is
folded when assigned. A column's folded display text is reused between its
filter and global search, and compiled group substring matches no longer fold
already-uppercased display text again.

- `source/Vittix.DBGrid.Filter.Engine.pas:311-343, 393-403`

### 3.3 [P] Export dialog preview limits — ✅ IMPLEMENTED 2026-11-04
The preview caps the exporter at 50 rows instead of building the entire export.
Each format captures `RecordCount` at most once for progress reporting, after
the export view is prepared; exports without a progress handler skip the read.

- `source/Vittix.DBGrid.Export.Dialog.pas`; `source/Vittix.DBGrid.Export.Engine.pas`

### 3.4 [P] `SyncColumnInfo` cross-scan — ✅ IMPLEMENTED 2026-11-04
Each layout sync builds a sorted, case-insensitive field-name index once, then
checks grid columns against it. This preserves the collection's first-match
behavior and avoids the O(columns × info) scan.

- `source/Vittix.DBGrid.pas` (`SyncColumnInfo`)

---

## Milestone 4 — Rendering & UX polish

### 4.1 [F] VCL Styles support — ✅ COMPLETED 2026-10-27 (v1.2.0, see B4)
### 4.2 [F] XLSX numeric cells — ✅ COMPLETED 2026-10-27 (v1.2.0, see B2)
### 4.3 [F] Implement or remove the PDF export stub — ✅ COMPLETED 2026-10-27
(v1.2.0, hidden from the dialog; see B3)

---

## Milestone 5 — Design-time experience

### 5.1 [F] Property editors — ✅ IMPLEMENTED 2026-11-04
The aggregation enum property editor uses friendly names. `CellConditions`
items display their field, operator, value, and disabled state.

### 5.2 [F] Published property categories — ✅ IMPLEMENTED 2026-11-04
Grid and column metadata properties are registered in Layout, Appearance, and
Behavior; grid storage paths are registered in Persistence.

### 5.3 [F] Palette glyph — ✅ IMPLEMENTED 2026-11-04
The design package links a 24-bit `TVITTIXDBGRID` bitmap resource from
`assets/palette/`.

### 5.4 [F] Design-time footer preview — ✅ IMPLEMENTED 2026-11-04
The grid paints a static footer band while in design mode and invalidates it
when `FooterVisible` changes. Visual inspection inside the IDE remains open.

---

## Milestone 6 — Docs, hygiene & distribution

### 6.1 [D] Fill in empty docs — ✅ UPDATED 2026-11-04
The docs index links to the main guides, demo, installer, changelog, and
license. This roadmap records the recently implemented items, and
CONTRIBUTING.md describes the flat `source/` layout and design-time unit paths.

### 6.2 [D] Reconcile version claims — ✅ UPDATED 2026-11-04
The README states Delphi 12 Athens on Win32 as the supported build target;
CONTRIBUTING and the installer notes refer to the same target. Multi-version
and 64-bit support require separate builds and verification. CI remains a
separate future task.

### 6.3 [D] Remove the madExcept dependency from the demo — ✅ COMPLETED 2026-09-29 (v1.0.4)
The madExcept units were removed from `VittixDBGridFullDemo.dpr` and the
`madExcept` define from the demo project; the demo now builds with a plain
Delphi installation.

### 6.4 [D] Repository hygiene — ✅ COMPLETED 2026-09-29 (v1.0.4)
- `*.drc` / `*.mes` were already ignored; done earlier.
- `source/Vittix.DBGrid.pas.bak` and the 0-byte `source/VittixDBGrid.res`
  removed; the `*.res` ignore rule was scoped so the intentional project
  resources stay tracked.
- Changelog-style noise comments ("FIX BUG n", "NEW:", "FIXED VERSION")
  were cleaned out of the source units; why-comments were kept.

### 6.5 [D] API documentation — ✅ UPDATED 2026-11-04
The grid, controller, and primary dialog entry points now have XML summaries
for their application-facing methods and settings. `docs/API.md` maps the
common entry points and persistence rules. A generated reference can be added
later if a documentation generator is adopted.

---

## Feature backlog (post-1.x, larger items)

- **Column freezing** (fixed left columns).
- **Grouping** (group rows + group footers).
- **Incremental search** (type-ahead locate).
- **Server-side filter builder** — generate WHERE clauses for FireDAC/ADO
  instead of `OnFilterRecord` display-text matching (the key EhLib-parity gap).
- **Pagination / virtual mode** for large datasets.
- **Printing / report export** built on the existing export engine.
- **Multi-select-aware operations** (copy selected rows, aggregate selection).

---

## Suggested order of execution

1. Milestone 1 entirely (all small, isolated, testable fixes).
2. 2.4 + 2.5 (dead code and exception hygiene — cheap, clears the way for 2.1).
3. 2.1 + 2.2 (the big refactor; add regression tests before starting — the
   existing `Controller.Regression` tests are the safety net).
4. Milestone 3 performance items (each independently shippable).
5. Milestone 6 hygiene (any time; ideally before the next release).
6. Milestones 4–5 and the feature backlog, prioritized by user demand.

---

# Future Development Plan (September 2026 review)

Fresh full-repo review after Milestones 1–2 landed (grid↔controller integration
is now virtual-override based; single source of truth for visual properties;
132 tests passing). This section supersedes the feature backlog above with a
phased plan. Each phase is independently shippable.

## Phase A — Feature foundation (small, unblocks everything)

- **A1 [F] Public notification events — ✅ COMPLETED 2026-09-04**
  - New typed events on the controller (implementation owner) with matching
    published surfaces on `TVittixDBGrid`: `OnAfterSort(Sender, Column)`,
    `OnFilterApplied(Sender, FieldName)`, `OnAfterApplyLayout`,
    `OnColumnMoved(Sender, Column, OldIndex, NewIndex)`. Both surfaces share
    one handler storage, so an operation fires exactly once. Sender is the
    controller.
  - Engine callbacks now reachable: `OnValidateFilter`, `OnFormatAggregation`,
    `OnFieldValidation` are public properties on the controller. Handlers are
    cached on the controller and pushed to the engines whenever they are
    (re)created, so assignment survives dataset churn. The filter popup also
    receives `OnValidateFilter` for live in-dialog validation.
  - Firing points: `DoTitleClick` (only after `ToggleSort` applied without
    raising), `ApplyState`/`Clear` (Column = nil), popup commit /
    `SetGlobalFilter` / `ClearFilters`, end of `ApplyLayout`, and column moves
    (chooser drag/keyboard/rollback, `ApplyLayout` restore, and the new
    `TVittixDBGrid.ColumnMoved` override for VCL-initiated moves).
  - Covered by `TVittixControllerEventTests` (assignment, invocation,
    no-duplicate, nil-safety with freed controller / closed dataset,
    coexistence with application event handlers).

- **A2 [R] Strongly typed `TVittixDBGrid.Controller` — ✅ COMPLETED 2026-09-04**
  - `Controller` is now `TVittixDBGridController` (public, not streamed — no
    DFM impact). To break the unit cycle, `Vittix.DBGrid.Controller` and
    `Vittix.DBGrid.FooterPanel` moved their `Vittix.DBGrid` dependency to the
    implementation section; the controller's `Grid` property and the footer's
    `Attach` are `TDBGrid`-typed with implementation-side `VittixGrid()`
    helpers, and `SetGrid` raises `EArgumentException` for non-Vittix grids
    (previously enforced only at compile time through the property type).
  - All ~15 `is TVittixDBGridController` casts removed from the grid, the
    design-time editor, the demo, and the tests.

- **A3 [R] Single filter-operator table — ✅ COMPLETED 2026-09-04**
  - One authoritative table in `Vittix.DBGrid.Filter.Engine`
    (`TVittixFilterOperatorDefinition` + parse/lookup functions, built once in
    the unit initialization). The engine's `ParseFilterMode`, the popup's
    combo fill / prefix generation / persisted-text restoration now all use
    it; the three hand-synced copies are gone. The table order is the combo
    order and the persisted `OperatorIndex` contract.
  - Observable syntax unchanged, including `!..` (Not Between) and
    length-delimited word operators (`nullity` stays a Contains filter).
    Side effect: the popup now splits stored text exactly like the engine
    (trimmed), so hand-set filter texts with leading spaces restore
    consistently. Covered by `Vittix.Tests.FilterOperators`.

- **Discovered while testing A1 [B] — fixed**: applying any filter through
  the controller silently undid itself. The engine's
  `DisableControls/EnableControls` around `Filtered := True` fires
  `deDataSetChange`, and `DataLinkDataSetChanged` responded by destroying the
  engines — the dying engine's destructor reset `Filtered` and unhooked
  `OnFilterRecord`. `DataLinkDataSetChanged` now checks dataset identity and
  only rebuilds when the dataset actually changed or closed (previously
  sorting survived this thrash by luck because index state lives on the
  dataset; filter state does not). This also removes most of the engine
  churn called out in C5.

- **A4 [F] Layout schema v2** — ✅ COMPLETED 2026-10-27 (v1.3.0). Versioned
  JSON: `version` field (v1 files load unchanged; unknown fields ignored),
  per-column pinned/titleCaption/displayFormat/exportInclude/exportCaption
  with matching `TVittixDBGridColumnInfo` properties, `Assign` support,
  ApplyLayout pushing captions/formats and the export engine honoring the
  export settings. Freezing (D3) builds on the Pinned flag.

## Phase B — Finish the half-built features (quick wins)

- **B1 [B] Implement `ExportFilteredOnly` and `IncludeFooter`** — ✅ COMPLETED
  2026-09-29 (v1.0.4). Both options are honored by every format (CSV, TSV,
  HTML, XLSX, XML, JSON, Text) through the shared iteration helpers, with
  cursor restore and regression tests.
- **B2 [F] XLSX numeric cells** — ✅ COMPLETED 2026-10-27 (v1.2.0). Explicit
  `t="n"` numeric cells (integer fields from their exact digits), `t="b"`
  booleans, Excel serial date/time/timestamp cells with date/time/datetime
  number formats from a new `xl/styles.xml` (content types and rels kept
  valid), NULLs as empty cells, `xml:space="preserve"` inline strings, and
  `<cols>` widths derived from the grid column widths (caption-length
  heuristic where no usable grid width exists). `ExportLocaleFormat := True`
  keeps the wholesale display-text behavior.
- **B3 [F] PDF: implement a minimal writer or hide it** — ✅ COMPLETED
  2026-10-27 (v1.2.0) with the hide option: the dialog never offers PDF, the
  enum member is kept for compiled-code compatibility and documented as not
  implemented, and `ExportToStream(vefPDF)` still raises.
- **B4 [F] VCL Styles support** — ✅ COMPLETED 2026-10-27 (v1.2.0). New
  `Vittix.DBGrid.Styles` unit (`TVittixStylePalette`) resolving system
  colors through per-control style services; footer panel, filter popup and
  export dialog follow the active style and stay pixel-identical unstyled.
- **B5 [F] Design-time polish** — Milestone 5 (property categories, palette
  glyph, aggregation property editor, design-time footer preview).

## Phase C — Performance (Milestone 3, unchanged) plus one addition

- Milestone 3.1–3.4 as written (incremental aggregation, uppercase cache,
  preview row cap, `SyncColumnInfo` index).
- **C5 [P] Stop destroying engines on every dataset change** —
  `DataLinkDataSetChanged` frees and recreates sort/filter/aggregation engines
  on each `DataSetChanged` notification (`Controller.pas:470-481`); reuse and
  rebind them instead.
- **C6 [P] Distinct-value scan and `RecordCount` polling** — ✅ UPDATED
  2026-11-04. The popup deduplicates with a hash lookup and sorts once,
  preserves the current record, and skips scanning during an edit or insert.
  Export progress reads a row total at most once per format when a progress
  handler is present (see 3.3). Enumerating all distinct values still requires
  a dataset scan when the popup opens.

## Phase D — New features (ordered by value/effort)

- **D1 [F] Incremental search (type-ahead locate)** — ✅ COMPLETED 2026-10-27
  (v1.2.0). `TVittixDBGrid.KeyPress` override routes printable characters to
  `Controller.DoKeyPress`; the buffer lives on the controller, locates via
  the primary sort column (or current column when unsorted) with a
  cursor-restoring fallback scan, resets on Esc/timeout, and shows as a
  footer hint. New published `IncrementalSearch`,
  `IncrementalSearchTimeout` and `OnIncrementalSearch`.
- **D2 [F] Clipboard selection operations** — ✅ COMPLETED 2026-10-27
  (v1.2.0). Ctrl+C / Ctrl+Shift+C copy the current cell or the selected rows
  as TSV through the exporter (`BuildCopyText`), with an optional
  `CopyIncludeHeaders` header row and per-column selection aggregates
  (Sum/Count) in the footer while a multi-row selection is active.
- **D3 [F] Column freezing (fixed left columns)** — ✅ COMPLETED 2026-10-27
  (v1.3.0). Built on `TCustomGrid.FixedCols` (re-applied after every layout
  change since TDBGrid resets it to the indicator offset): published
  `FrozenColumns` with clamping, `FreezeUpToColumn`/`UnfreezeAll`/
  `RefreshFrozenColumns`, `OnFrozenColumnsChanged`, drag-boundary
  enforcement with rollback in the ColumnMoved override and the chooser,
  footer `GetColumnRect`/hit-test alignment under frozen columns, and
  per-column `Pinned` persistence via the v2 layout schema. There is no
  header context menu in the grid, so the freeze operations are exposed as
  API only.
- **D4 [F] Richer in-place editors** — ✅ COMPLETED 2026-10-27 (v1.4.0).
  Per-column `EditorKind` (pick list with value=caption pairs, searchable
  lookup from the field's LookupDataSet, masked numeric with Min/Max and
  DecimalPlaces), `Required` and the `OnValidateCell` validation gate
  shared by all Vittix editors; options persist through the layout.
- **D5 [F] Saved filter sets + filter builder** — ✅ COMPLETED 2026-10-27
  (v1.4.0, with the date-aware comparisons landed in 1.1.1). Named sets
  persisted per grid as JSON, OR-groups in the engine with precompiled
  conditions, popup Saved menu + manager dialog, layout integration.
  Remaining builder niceties: an OR-group editing surface inside the filter
  popup itself (today groups are managed in the set manager).
- **D6 [F] Grouping with group footers** — the largest architectural lift.
  The footer panel is a manually positioned sibling control
  (`FooterPanel.pas:105-168`); grouping needs a proper band model. The
  aggregation engine is already filter-aware and can back group aggregates.
- **D7 [F] Server-side filter/sort provider** — ✅ COMPLETED 2026-10-28
  (v2.0.0). `Vittix.DBGrid.Provider` (SQL builder for six dialects +
  `IVittixDataProvider`), `Vittix.DBGrid.Provider.FireDAC` in a separate
  `VittixDBGridControllerFD` package, opt-in `FilterMode`
  (client/server/auto) with client fallback and `OnProviderFallback` /
  `OnProviderError`. Remaining ideas: providers for ADO/dbGo and
  UniDAC (the provider interface is public), a query-shape parser that
  could wrap top-level ORDER BY safely, and async re-query.
- **D8 [F] HTML print preview — implemented 2026-11-04.** The HTML exporter
  includes print CSS with repeated table headers and row page-break rules.
  `OpenHTMLPrintPreview` saves to a caller-chosen file and opens the system
  browser; the feature demo exposes it. Native printer integration and PDF
  output remain separate future work.
- **D9 [F] Pagination / virtual mode — FireDAC paging slice implemented
  2026-11-04.** `TVittixFireDACPager` offers explicit, zero-based query pages
  with query-state restoration and edit guards. A reusable VCL pager bar
  now supplies First, Previous, and Next controls. The core package also
  includes `TVittixPagedDataSet`, an explicit page-backed dataset with a
  caller-supplied loader for other backends, and a matching VCL navigation
  bar. Opt-in `AutoPageOnScroll` now crosses page boundaries during normal
  dataset/grid navigation while keeping one page loaded. Optional
  `TVittixPagedDataSet.TotalCount` (implemented 2026-11-05) lets the loader
  or caller supply a total row count: when known, the navigation bar shows
  "Page X of Y" and a Last button, navigation trusts the counted boundary
  instead of sending empty-page probes, and count changes refresh the loaded
  page's has-more state. With the count unknown the probe-based behavior
  above is unchanged, and the FireDAC pager stays count-free so its bar
  keeps Last disabled.
  A bounded global scrollbar slice (implemented 2026-11-05) rides on the
  known count: opt-in `TVittixPagedDataSet.VirtualScroll` exposes the total
  as `RecordCount` and the global row as `RecNo` — the two values
  `TCustomDBGrid` sizes and positions its scrollbar from — so the bound
  grid's scrollbar covers every source row while one page stays in memory;
  `RecNo` assignment seeks across pages and `TVittixDBGrid` routes scrollbar
  endpoints and Ctrl+Home/End through `GlobalFirst`/`GlobalLast` via the
  `IVittixGlobalPageScroll` interface. Hardening (2026-11-05, verified by
  tests on a visible grid): a client `Filtered` view suspends the mode
  (global positions have no truthful meaning for a filtered page);
  bookmarks in virtual mode are page-scoped — a verified CDS behavior lets
  native bookmarks silently reposition onto the same local row of a
  replaced page, so `GetBookmark` now returns a tagged wrapper. Its
  generation rejects bookmarks after any page reload, including a reload
  of the same page index, and a second audit (2026-11-05) reproduced and
  fixed the toggle seams: a wrapper survived `Filtered` or `VirtualScroll`
  toggles and could reposition onto a different record in the renumbered
  (filtered) view, so validity now also requires the virtual mode to be
  active — stale use raises `EDatabaseError`,
  `BookmarkValid`/`SelectedRows.Refresh` prune stale selections, and an
  all-stale multi-select delete was verified to refuse before deleting
  any row; export was verified to emit exactly the loaded
  page and restore the cursor; count shrink/grow and edit-mode seek
  refusal were verified at grid level. A third audit (2026-11-05)
  reproduced the mixed-selection seam: the VCL `TBookmarkList.Delete`
  iterates its sorted list from the end and raises only when it reaches a
  stale entry, so a selection mixing valid current-page and stale
  cross-page bookmarks partially deleted before raising. The grid's
  Ctrl+Delete path now validates every entry first and refuses the whole
  delete (`EDatabaseError`, all-or-nothing, verified), and a public
  `TVittixDBGrid.SelectionHasStaleEntries` lets callers show a stale
  selection warning. The public `DeleteSelectedRowsSafely` now validates
  every bookmark before delegating deletion to VCL, including for ordinary
  datasets. Direct `SelectedRows.Delete` retains the raw VCL behavior.
  Export progress now reads the loaded-page row count while virtual scroll
  is active, with `MaxRows` still capping the total; CSV/XLSX progress,
  cursor preservation, and no extra page load are covered by DUnitX.
  Runnable demo (`demos/paged-demo`, 2026-11-05): known count +
  `VirtualScroll` + navigation bar over a simulated 1,234-row source, with
  a console self test that drives the visible grid (scrollbar range and
  position, bar page changes, stale-selection refusal and pruning, exact
  valid-selection deletion, virtual-scroll toggle). Building the demo
  surfaced and fixed two core defects: the filter engine engaged on
  condition-less applies during dataset churn and silently set `Filtered`
  on the bound paged dataset (now scoped: condition-less applies skip the
  hook only for page-backed datasets), and `OpenPage`'s EmptyDataSet +
  reload churn displaced MIDAS packet state under a live visible grid —
  page loads now build into an internal scratch dataset and adopt the
  packet through one atomic `Data` assignment; virtual-mode bookmarks
  embed the native CDS bookmark (record identity) behind the page/generation
  scope so navigation lands exactly regardless of grid window management.
  The architectural boundary found in
  the VCL source stands: `TDataSet.First`/`Last` are non-virtual (and their
  `InternalFirst`/`InternalLast` hooks are also driven by buffer BOF/EOF
  flags), so they stay page-local. The `TVittixGlobalNavigator` subclass
  now routes First/Last buttons through global seeks while virtual scroll
  is active; a stock `TDBNavigator` remains page-local.

## Phase E — Ecosystem & distribution

- **Delphi 12 Athens/Win32 CI — configured 2026-11-04.** A GitHub Actions
  workflow builds the runtime, FireDAC, and design packages and runs DUnitX on
  a labeled Windows self-hosted runner. Package builds and the test command
  were verified locally; an actual hosted run awaits runner registration and
  a push. Verify other compiler and platform combinations before expanding
  the supported target.
- **Installer path/platform plumbing — updated 2026-11-04.**
  `build-installer.ps1` already accepts BPL/DCP source directories. It now
  derives the payload platform from the build platform and rejects mismatches;
  the Inno Setup task text and library registry path use that platform. The
  helper now executes and checks both package builds (its former multi-line
  `cmd /c` skipped them). The setup excludes local build and demo-state
  artifacts and includes the design palette resource. The default Delphi
  12/Win32 installer compiled locally.
  Multi-version payloads
  (D10.4–12, Win32 + Win64) still need separate builds and verification.
- Milestone 6.3 (madExcept-free demo) and 6.4 (hygiene: `.pas.bak`, `.dcu`
  artifacts, `.gitignore` for `.drc`/`.mes`), 6.5 (API docs).

## Recommended sequence

1. A1–A3 (foundation; each ~1 day, regression tests exist).
2. B1–B3 (close dead options, XLSX quality, PDF decision) + 6.4 hygiene →
   tag **v1.1**.
3. D1 + D2 (small user-visible features) + B4/B5 → tag **v1.2**.
4. C (performance) → tag **v1.3**.
5. A4 + D3 (column freezing) → tag **v2.0**.
6. D6 (grouping) and D7 (server-side) as the flagship v2.x tracks.
7. E runs continuously; CI before the next public release.

## October 2026 regression follow-up

- Item 14: design completed 2026-10-05; incremental code skipped because the
  current notification contract does not supply reliable committed deltas.

- Item 13: completed 2026-10-05; typed date/time/timestamp comparisons,
  locale parsing, null handling and display-text fallback regression tests.

- Item 12: completed 2026-10-05; TStopwatch-throttled message pumping,
  disabled grid during export, reentry rejection and exception-safe restore.

- Item 11: completed 2026-10-05; WindowProc restoration checks ownership.
  Later chained hooks must detach before controller destruction.

- Item 10: completed 2026-10-05; repository search found no fixed-row message
  sender; the dead constant and swallow branch were removed.

- Item 9: completed 2026-10-05; full layout reset and font-based widths.

- Item 8: completed 2026-10-05; layout persistence creates parent directories.

- Item 7: completed 2026-10-05; filter unhook checks handler identity.

- Item 6: completed 2026-10-05; unconditional memo reads removed from drawing.

- Item 5: completed 2026-10-05; edit-safe scans and controller-mediated footer
  changes retain dirty state until the edit completes. Same-dataset refresh
  now also processes those deferred changes without rebuilding engines.

- Item 4: investigated 2026-10-05; not reproduced. Closing the owned
  TClientDataSet during destruction tears down the controller engines before
  its memory is released. Active-filter owner-first teardown is guarded.

- Item 3: completed 2026-10-05; header input requires visible titles.

- Item 2: completed 2026-10-05; layout refresh and batched column updates.

- Item 1: investigated 2026-10-05; not reproduced with TClientDataSet.
  `AggregatesRefreshAfterDeleteAndPost` guards totals and engine identity.

### Count/Sum incremental aggregation design - 2026-10-05

The present TDataLink RecordChanged callback is a view-change notification,
not a committed mutation log. Scrolling, filter/index changes and control
re-enabling can produce overlapping callbacks. During edits Field.NewValue
does not establish whether a later Post or Cancel commits that value; after
Delete the removed row is no longer available. Applying deltas from these
callbacks could silently double-count changes or retain canceled values.

A future implementation needs a dataset adapter that reports committed
insert/update/delete mutations with stable row identity, old/new typed values
and a monotonic mutation token. It must preserve the virtual grid integration
and application dataset handlers. Datasets without this contract retain the
full-scan path. A generic adapter cannot assume bookmarks or OldValue have
consistent meanings across TClientDataSet, FireDAC and custom datasets.

For Count/Sum, cache committed contributions for supported numeric fields.
Count must retain the existing non-null field-count semantics; currency and
integer sums must keep their native accumulators rather than convert to
Double. Apply a delta exactly once only after a successful commit, and only
while the cached view generation still matches. Cancel and scroll contribute
no delta. Unsupported fields, missing old values, invalid identities and
uncertain filter membership invalidate the cache and trigger a full scan.

Filter, global search, sort, layout, dataset replacement, close/reopen and
aggregation-type changes invalidate the generation. Full scans remain guarded
against recursion and defer during dsEdit/dsInsert. A full scan creates the
baseline before incremental updates resume. No incremental state may survive
dataset destruction.

Before shipping, compare incremental results to independent full scans for
Post/Delete/Insert/Cancel, null transitions, filter entry/exit, sort/layout
changes, close/reopen, failed validation and duplicate notifications. Add
scan-count instrumentation to prove updates avoid scans without weakening
those correctness checks. This design adds no event hooks or runtime code.
