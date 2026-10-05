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
`DataLinkRecordChanged` marks aggregation dirty on every record change, and
refresh triggers a full dataset scan. For Count/Sum/Min/Max it is cheap to
update accumulators incrementally on `Field.NewValue`/post/delete when only one
record changed; fall back to full recalculation on filter/sort changes.

- `source/Vittix.DBGrid.Controller.pas:525-539, 805-828`; `Aggregation.Engine.pas:250-328`

### 3.2 [P] Precompute uppercase in filter matching
`MatchFilter` re-uppercases both strings per field per record. Cache the
uppercased needle at `ApplyFilter` time and uppercase the haystack once per
record across all columns.

- `source/Vittix.DBGrid.Filter.Engine.pas:311-343, 393-403`

### 3.3 [P] Export dialog preview limits
The preview builds the ENTIRE export into a string before truncating display to
50 lines. Add a row cap (e.g. first 100 rows) to `TVittixDBGridExporter` and
generate the preview from it. Also stop polling `RecordCount` every 100 rows.

- `source/Vittix.DBGrid.Export.Dialog.pas:446-494`; `Export.Engine.pas:571`

### 3.4 [P] `SyncColumnInfo` cross-scan
Runs on every `LayoutChanged` with an O(columns × info) scan. Maintain the
collection incrementally or keep a field-name index on `TVittixDBGridColumns`.

- `source/Vittix.DBGrid.pas:370-393`; `ColumnInfo.pas:151` (`FindByFieldName`)

---

## Milestone 4 — Rendering & UX polish

### 4.1 [F] VCL Styles support
The footer panel hard-codes classic colors (`clBtnHighlight`/`clBtnShadow`,
`FooterPanel.pas:271-289`) and the filter popup forces `clBtnFace`
(`Filter.Popup.pas:149`). Use `TStyleManager.Style`/`StyleServices` for
backgrounds, fonts and edges so styled applications don't get a 1997-looking
footer. Optionally use theme edges (`DrawThemeEdge`).

### 4.2 [F] XLSX numeric cells
All cells are written as `inlineStr`, and there is no `xml:space="preserve"`,
so numbers arrive as text in Excel and leading/trailing spaces can be dropped.

- `source/Vittix.DBGrid.Export.Engine.pas:1038-1068`
- Fix: emit `<c t="n"><v>` for numeric/boolean field types; add
  `xml:space="preserve"` to inline strings. (Or document the limitation
  prominently if deferred.)

### 4.3 [F] Implement or remove the PDF export stub
`vefPDF` is advertised in the enum and dialog but raises at runtime
(`Export.Engine.pas:486`). Either implement it or hide it from the dialog until
ready.

---

## Milestone 5 — Design-time experience

### 5.1 [F] Property editors
- Enum property editor for `TVittixAggregationType` with friendly names.
- Collection editor niceties for `CellConditions` (display name shows
  field + operator + value).

### 5.2 [F] Published property categories
Organize new published properties with `RegisterPropertyInCategory` (or
category attributes) — Layout, Appearance, Persistence groups.

### 5.3 [F] Palette glyph
Add a proper `.dcr`/24-bit palette bitmap for `TVittixDBGrid`.

### 5.4 [F] Design-time footer preview
Currently the footer never appears in the IDE (`SetShowFooter` design-guarded,
`Controller.pas:381`). A static design-time preview would make `FooterVisible`
visible to users at design time.

---

## Milestone 6 — Docs, hygiene & distribution

### 6.1 [D] Fill in empty docs
- `docs/README.md` and `docs/ROADMAP.md` (this file) were committed empty —
  keep them current.
- CONTRIBUTING.md describes a `source/core|sorting|filtering` layout that does
  not exist (flat `source/`) — fix the description.

### 6.2 [D] Reconcile version claims
README says Delphi 10.3+, CONTRIBUTING says 10.4+, installer ships only a
Delphi 12/Win32 payload. Decide the real minimum, state it once, and ideally
verify with CI builds.

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

### 6.5 [D] API documentation
Add XML doc comments consistently (engines have them; grid/controller/dialogs
mostly don't) and consider generating an API reference from them.

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

- **A4 [F] Layout schema v2** (versioned JSON): add pinned/fixed flag, title
  caption, format/DisplayFormat, per-column export settings (include,
  caption override, mask). Today only fieldName/displayIndex/width/visible/
  sort/aggregation/footerText/cellConditions round-trip
  (`Layout.pas:152-160`). Needed by D3 (freezing) and export quality work.
  Still pending.

## Phase B — Finish the half-built features (quick wins)

- **B1 [B] Implement `ExportFilteredOnly` and `IncludeFooter`** — ✅ COMPLETED
  2026-09-29 (v1.0.4). Both options are honored by every format (CSV, TSV,
  HTML, XLSX, XML, JSON, Text) through the shared iteration helpers, with
  cursor restore and regression tests.
- **B2 [F] XLSX numeric cells** — emit `<c t="n">` for numeric/boolean field
  types, `xml:space="preserve"` on inline strings, and column widths
  (`Export.Engine.pas:1050-1203`). Milestone 4.2.
- **B3 [F] PDF: implement a minimal writer or hide it** from the dialog
  (`Export.Engine.pas:498` raises). Milestone 4.3.
- **B4 [F] VCL Styles support** for footer panel + filter popup
  (Milestone 4.1).
- **B5 [F] Design-time polish** — Milestone 5 (property categories, palette
  glyph, aggregation property editor, design-time footer preview).

## Phase C — Performance (Milestone 3, unchanged) plus one addition

- Milestone 3.1–3.4 as written (incremental aggregation, uppercase cache,
  preview row cap, `SyncColumnInfo` index).
- **C5 [P] Stop destroying engines on every dataset change** —
  `DataLinkDataSetChanged` frees and recreates sort/filter/aggregation engines
  on each `DataSetChanged` notification (`Controller.pas:470-481`); reuse and
  rebind them instead.
- **C6 [P] Distinct-value scan and `RecordCount` polling** in the filter
  popup (`Filter.Popup.pas:535-586`) and export progress
  (`Export.Engine.pas:571, 583`).

## Phase D — New features (ordered by value/effort)

- **D1 [F] Incremental search (type-ahead locate)** — small: the `KeyDown`
  override already routes to the controller; add a search buffer + `Locate` +
  optional status hint.
- **D2 [F] Clipboard selection operations** — copy cell/row/multi-selection,
  aggregate the selection in the footer; reuse the existing clipboard
  exporter. (Backlog: "multi-select-aware operations".)
- **D3 [F] Column freezing (fixed left columns)** — requires A4 schema plus
  fixed-region awareness in footer `GetColumnRect` and layout apply
  (`FooterPanel.pas:193-227`, `Controller.pas:944`). Highest user-visible
  value after search.
- **D4 [F] Richer in-place editors** — dropdown/picklist, lookup, masked
  numeric, per-column validation (`Editors.pas` currently: memo + date only).
- **D5 [F] Saved filter sets + filter builder** — named filter profiles per
  grid, OR-groups between column filters, date-aware operators (dates are
  compared as text/float today).
- **D6 [F] Grouping with group footers** — the largest architectural lift.
  The footer panel is a manually positioned sibling control
  (`FooterPanel.pas:105-168`); grouping needs a proper band model. The
  aggregation engine is already filter-aware and can back group aggregates.
- **D7 [F] Server-side filter/sort provider** — generate FireDAC/ADO
  `WHERE`/`ORDER BY` instead of `OnFilterRecord` display-text matching
  (the key EhLib-parity gap; `Sort.Engine.pas:163-198` already has the
  FireDAC dialect note).
- **D8 [F] Printing / print preview** built on the export engine (HTML
  preview first; shares B3 PDF work).
- **D9 [F] Pagination / virtual mode** for very large datasets.

## Phase E — Ecosystem & distribution

- CI builds (the DUnitX console runner already exits non-zero on failure —
  CI-ready); verify the real minimum Delphi version (README says 10.3+,
  CONTRIBUTING 10.4+, installer ships Delphi 12/Win32 only) — Milestone 6.2.
- Multi-version installer payloads (D10.4–12, Win32 + Win64); un-hardcode
  `build-installer.ps1` BPL source paths.
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
# October 2026 regression follow-up

- Item 1: investigated 2026-10-05; not reproduced with TClientDataSet.
  `AggregatesRefreshAfterDeleteAndPost` guards totals and engine identity.
