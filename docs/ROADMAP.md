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

### 1.6 [B] Clean up temp file on failed atomic export
`ExportToFileAtomic` leaves `TempFileName` behind when the export proc raises,
and its delete-then-move is not atomic.

- `source/Vittix.DBGrid.Export.Engine.pas:449-461`
- Fix: `try..except`/`finally` around the export that deletes the temp file on
  failure; move to a unique temp name in the target directory and use
  `TFile.Replace` where possible.

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

### 2.1 [R] Replace event-handler hooking with virtual overrides
The controller saves/replaces `OnTitleClick`, `OnDrawColumnCell`, `OnMouseDown`,
`OnDblClick`, `OnKeyDown` and the `WindowProc` on the grid. Any later assignment
to those properties by application code silently unhooks sorting/colors, and
stacked hooks are a recurring source of ordering bugs.

- `source/Vittix.DBGrid.Controller.pas:395-460` (`HookGrid`/`UnhookGrid`), `:593-615` (`GridWindowProc`)
- Fix: override the corresponding virtual methods in `TVittixDBGrid`
  (`TitleClick`, `DrawCell`, `MouseDown`, `DblClick`, `KeyDown`) and call the
  controller from there; fire NEW public events (`OnAfterSort`,
  `OnFilterApplied`, `OnAfterApplyLayout`) so consumers no longer need to reach
  into the controller's engines. Keep the WindowProc hook only for footer sync
  messages.

### 2.2 [R] Eliminate the protected-member crackers
`TVittixGridHelper = class(TCustomDBGrid)` and `TVittixGridAccess = class(TDBGrid)`
reach protected members from outside. Since `TVittixDBGrid` is our own
descendant, promote the needed members (`LeftCol`, `CellRect`,
`IndicatorOffset`) as public utilities on `TVittixDBGrid` instead.

- `source/Vittix.DBGrid.Controller.pas:50`, `source/Vittix.DBGrid.FooterPanel.pas:88`

### 2.3 [R] Single source of truth for visual properties
The grid keeps local copies of `FooterVisible`/`AlternatingRowColors`/
`AlternateRowColor` AND pushes them to the controller. Replace the duplicated
fields with direct delegation to the controller (keep published property
signatures unchanged for DFM compatibility).

- `source/Vittix.DBGrid.pas:34-35, 129-132, 210-246`

### 2.4 [R] Delete dead scaffolding
- Empty `TraceGrid`/`TraceController` stubs called ~30 times
  (`Vittix.DBGrid.pas:115`, `Controller.pas:216`).
- Commented-out `DebugMsg` (`Controller.pas:211-214`).
- Duplicated/dead `FLineBreak` option (`Export.Engine.pas:216-218`).
- Dead `Lines.Add` before overwrite in preview (`Export.Dialog.pas:450-453`).
- Public raw field `Aggregation: TVittixAggregation` → property
  (`ColumnInfo.pas:112`).
- Redundant RTTI `SetPropValue(..., 'IndexName', ...)` paths
  (`Sort.Engine.pas:277, 307, 323`).

### 2.5 [R] Exception hygiene
- Replace bare `except end` blocks with targeted exception types or logging:
  `Sort.Engine.pas:232-235, 309-312`; `Filter.Popup.pas:658-668`.
- Dedicated `EVittixExportError` for the PDF stub instead of generic
  `Exception.Create` (`Export.Engine.pas:486`).

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

### 6.3 [D] Remove the madExcept dependency from the demo
`demos/features-demo/VittixDBGridFullDemo.dpr` requires the commercial
madExcept suite, breaking compilation for anyone without it. Make it
`{$IFDEF}`-optional or remove.

### 6.4 [D] Repository hygiene
- Add `*.drc`, `*.mes` to `.gitignore` (currently untracked-but-present).
- Remove `source/Vittix.DBGrid.pas.bak`; ensure builds output to a `build/`
  folder instead of littering `source/` with `.dcu`s.
- The three committed `.res` files conflict with the `*.res` ignore rule —
  either document them as intentional (version info) or scope the ignore rule.

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
