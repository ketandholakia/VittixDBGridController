# API guide

The main application entry point is `TVittixDBGrid` in
[`source/Vittix.DBGrid.pas`](../source/Vittix.DBGrid.pas). It is a `TDBGrid`
subclass. Drop it on a VCL form, assign `DataSource`, then use its published
properties for appearance and routing. Operational methods live on the
grid-owned `Controller`.

## Grid settings

| Member | Purpose |
| --- | --- |
| `ColumnInfo` | Per-field sort, filter, aggregation, display, editor, and export metadata; match entries by `FieldName`. |
| `FooterVisible`, `AlternatingRowColors`, `AlternateRowColor` | Footer and row appearance. |
| `FrozenColumns`, `FreezeUpToColumn`, `UnfreezeAll`, `RefreshFrozenColumns` | Keep a leading run of visible columns fixed. |
| `IncrementalSearch`, `IncrementalSearchMode`, `IncrementalSearchTimeout` | Type-ahead locate behavior. `IncrementalSearchMode` is `ismAuto` (search only when the grid cannot edit; default), `ismAlways`, or `ismOff`. |
| `CopyIncludeHeaders` | Include headers for plain Ctrl+C; Ctrl+Shift+C includes them regardless. |
| `FilterMode` | `vcmClient` (default), `vcmServer`, or `vcmAuto` routing. Set `Controller.Provider` before using server routing. |
| `LayoutStorageFileName`, `ChooserStateFileName`, `FilterHistoryFileName`, `FilterSetsFileName`, `PersistenceRootPath` | Per-grid persistence locations. |

The grid republishes controller events such as `OnAfterSort`,
`OnFilterApplied`, `OnSortError`, `OnFilterSetApplied`, and `OnValidateCell`.
Assignments through either object reach the same handler. See the published
event declarations in [`Vittix.DBGrid.pas`](../source/Vittix.DBGrid.pas) for
the complete set.

## Controller operations

Use `Grid.Controller` for these operations:

| Task | API |
| --- | --- |
| Recalculate footer values and repaint | `Refresh` |
| Apply current sort state | `ApplyState` or `RefreshSorting` |
| Set global filter text | `SetGlobalFilter(Text)` |
| Re-route modified filter metadata | `RefreshFiltering` |
| Remove active filters | `ClearFilters` |
| Remove filters and sorting | `Clear` |
| Set a column's footer aggregation | `SetColumnAggregation(Column, Aggregation)` |
| Open the column chooser | `ShowColumnChooser` |
| Copy current cell or selected rows as TSV | `CopySelectionToClipboard(IncludeHeaders)` |
| Save, apply, or delete named filter sets | `SaveCurrentFilterAs`, `ApplyFilterSet`, `DeleteFilterSet` |

`CaptureLayout` and `ApplyLayout` transfer layout state objects. The stream
methods `SaveLayoutToStream` and `LoadLayoutFromStream` write and read layout
JSON. `SaveLayoutToFile` and `LoadLayoutFromFile` resolve an empty file name
from `LayoutStorageFileName`, then `PersistenceRootPath\layout.json`. They
raise `EVittixLayoutError` when neither path is configured; loading a missing
file is a no-op. `ResetLayout` restores default column order, visibility, and
widths and clears sorting, filters, and footer aggregates.

## Dialogs and export

`TfrmExportDialog.Execute(Grid)` opens the export dialog for a grid with an
active dataset. `TVittixDBGridColumnChooserForm.Execute(Grid)` opens the
column chooser. `TVittixDBGridFilterPopup.Execute(Owner, ColumnInfo)` edits a
column filter. These dialogs are also reached through the grid and controller
UI. For direct export without a dialog, use `TVittixDBGridExporter` in
[`source/Vittix.DBGrid.Export.Engine.pas`](../source/Vittix.DBGrid.Export.Engine.pas).
Its `Options.MaxRows` caps exported rows when positive; zero means unlimited.
`OpenHTMLPrintPreview(FileName)` writes print-friendly HTML to the chosen path
and opens it in the system browser. Browser printing (for example, Ctrl+P)
uses the HTML page's repeated table headers and page-break rules.

## FireDAC query paging

`TVittixFireDACPager` in `Vittix.DBGrid.Paging.FireDAC` provides opt-in,
zero-based pages for a `TFDQuery` bound to the grid:

```pascal
uses Vittix.DBGrid.Paging.FireDAC;

Pager := TVittixFireDACPager.Create(FDQuery1, 200);
try
  Pager.OpenPage(0);
  if Pager.NextPage then
    ShowMessage(Format('Page %d', [Pager.PageIndex + 1]));
  Pager.PreviousPage;
  Pager.Disable;
finally
  Pager.Free;
end;
```

Use a stable `ORDER BY` with a unique tie-breaker in the query. `OpenPage`
returns `False` for an empty page after page zero and restores the previous
page; page zero may be empty. `Disable` or destruction restores the query's
original `RecsSkip`, `RecsMax`, and open state. Post or cancel edits before
navigating or disabling. The pager is independent of the grid's provider
filter/sort state; after changing a filter or sort, reopen page zero.
Exports and client-side operations see the current page. FireDAC may enforce
the row limit on the client when the database cannot push it into SQL. This
is query paging, not a virtual-grid implementation, and it requires the
separate `VittixDBGridControllerFD` package.

For navigation buttons, create `TVittixFireDACPagerBar` from
`Vittix.DBGrid.Paging.Controls` and bind it to the pager:

```pascal
Bar := TVittixFireDACPagerBar.Create(Self);
Bar.Parent := Self;
Bar.Align := alBottom;
Bar.Pager := Pager;
```

The bar provides First, Previous, and Next buttons and shows the
one-based page number. It disables navigation when the pager is freed. Call
`Bar.UpdatePageState` after changing pages directly through `Pager`. The bar
always renders a Last button too, but the FireDAC pager has no total row
count, so Last stays disabled there.

## Page-backed datasets from other sources

`TVittixPagedDataSet` in `Vittix.DBGrid.Paging.DataSet` lets any backend load
one page into a grid-bound `TClientDataSet`. Define its `FieldDefs`, set
`PageSize` and `OnLoadPage`, then call `OpenPage(0)` and assign it to a
`TDataSource`. The callback receives the zero-based page index, page size,
and dataset to fill with at most `PageSize` records. `NextPage` and
`PreviousPage` navigate; an empty page after page zero returns `False` and
keeps the previous page. A loader exception also restores the previous
page, even if the loader inserted some records first. Post or cancel edits
before navigation. If the first load fails, the dataset returns to its
original closed state and can be retried. Choose `PageSize` before the first
page load.

This supports explicit pages over custom services or databases without
requiring FireDAC. The dataset holds the current page, so grid scrolling,
client filtering, export, and aggregates operate on that page. It does not
provide transparent infinite scrolling.

The optional `TotalCount` property supplies a total source row count — for
example, from a `SELECT COUNT(*)` the loader runs alongside the page query.
While the count is unknown (the default), navigation stays probe-based as
described above. Once the count is set, it becomes the page boundary
authority: `PageCount` derives from it (never below one, so an empty source
still has its single page and the bar shows "Page 1 of 1"), `NextPage`
returns `False` at the counted last page without sending an empty-page
probe to the source, and `LastPage` opens the final page directly. The
caller must keep the count current: a stale count stops navigation at the
counted boundary even if the source has grown, and setting a new count
while a page is loaded recomputes that page's has-more state (clearing it
with -1 restores the probe-based behavior). `TotalCountKnown` reports
whether a count is set.

`TVittixPagedDataSetBar` in `Vittix.DBGrid.Paging.DataSet.Controls`
provides the same First, Previous, and Next buttons without a FireDAC
dependency. Create it with a form owner, set `Parent`, and assign
`Bar.DataSet := PagedDataSet`. If code changes pages directly on the dataset,
the bar updates automatically. It disables Next when a page contains fewer
than `PageSize` rows or an empty next page has been reached. While
`TotalCount` is known, the caption reads "Page X of Y" and a Last button
jumps to the final page; otherwise Last stays disabled and the caption
keeps the "Page X" form.

Set `PagedDataSet.AutoPageOnScroll := True` to advance when grid or dataset
navigation moves past the current page boundary, and to load the previous
page when moving backward past its start. The property defaults to `False`.
The bound dataset still holds one page, and the grid scrollbar remains local
to that page. Scans that disable dataset controls, including the grid's
aggregation and export paths, stay on the current page. This is automatic
page-boundary navigation, not a global virtual scrollbar.

## Global scrollbar over a known total

With a known `TotalCount`, `TVittixPagedDataSet.VirtualScroll` (default
`False`) turns the bound grid's vertical scrollbar into one that spans
every source row while the dataset still holds only one page:

```pascal
PagedDataSet.TotalCount := RunCountQuery;   // keep this current
PagedDataSet.VirtualScroll := True;
PagedDataSet.OpenPage(0);
```

The dataset exposes the total as `RecordCount` and the global row position
(1-based, `PageIndex * PageSize + local row`) as `RecNo`, which is exactly
what `TCustomDBGrid` uses to size and position its scrollbar. Assigning
`RecNo` seeks across pages — the dataset loads the page containing the
target row and positions inside it, so scrollbar thumb drags, `MoveBy`, and
grid navigation all work at global scale. `TVittixDBGrid` additionally
routes scrollbar endpoints and Ctrl+Home / Ctrl+End through `GlobalFirst` /
`GlobalLast` (the `IVittixGlobalPageScroll` interface in
`Vittix.DBGrid.Paging.GlobalScroll`), because those paths call
`TDataSet.First`/`Last`.

A runnable demo of this mode (known count, `VirtualScroll`, the
navigation bar, selection behavior, and a console self test that verifies
the visible grid's scrollbar and page changes) lives in
[`demos/paged-demo`](../demos/paged-demo/README.md).

Known boundaries of this mode:

- `TDataSet.First`/`Last` are non-virtual and stay **page-local** even in
  virtual mode (they land on the first/last row of the currently loaded
  page). Direct calls and a stock `TDBNavigator` keep that behavior.
  `TVittixGlobalNavigator` in `Vittix.DBGrid.Paging.DataSet.Controls`
  routes its First/Last buttons to `GlobalFirst`/`GlobalLast` when virtual
  scrolling is active, and otherwise inherits the stock navigator actions.
  Bind its `DataSource` to the paged dataset. Prior/Next remain page-local
  unless `AutoPageOnScroll` is enabled.
- `RecordCount` means the total, not the loaded rows. Code that needs the
  loaded row count cannot read it from `RecordCount` while the mode is on.
- **Client filters suspend the mode.** Filtering is page-local by design,
  and global row positions have no truthful meaning for a filtered view,
  so an active `Filtered` (including the grid's filter engine) reverts
  `RecordCount`/`RecNo` to page-local semantics until the filter clears.
- **Bookmarks are page-scoped and mode-scoped.** In virtual mode
  `GetBookmark` returns a tagged wrapper; `BookmarkValid` reports `False`
  after any successful page reload (even the same page index loaded again
  with different rows) **and while the virtual mode is not active** — a
  `Filtered` toggle or `VirtualScroll := False` marks existing wrappers
  stale, because the stored local row would name a different record (in a
  filtered view it renumbers the visible rows). Clearing the filter
  without reloading the page restores the wrapper's validity.
  `GotoBookmark` on a stale wrapper raises `EDatabaseError` instead
  of the silent repositioning the native CDS bookmark performs (verified:
  a stale native bookmark lands on the same local row of the new page).
  `SelectedRows.Refresh` prunes stale selection entries.

  **Selection deletion is all-or-nothing on the grid's keyboard path.**
  The VCL's `TBookmarkList.Delete` iterates its sorted list from the end
  and raises only when it *reaches* a stale entry, after already deleting
  every entry processed before it. With a mixed selection (valid current-
  page entries plus stale cross-page ones) that is a partial deletion —
  reproduced and then fixed for the grid: `TVittixDBGrid`'s Ctrl+Delete
  path validates every entry first and refuses the whole operation with
  `EDatabaseError` before any row is deleted (verified: the valid current-
  page row survives the refusal). Application code should call
  `Grid.DeleteSelectedRowsSafely` for selected-row deletion: it validates
  every bookmark first, raises `EDatabaseError` without deleting a row if
  any entry is invalid, and then uses the VCL delete path for a valid
  selection. It works with ordinary datasets too. Direct
  `Grid.SelectedRows.Delete` retains the VCL partial-delete risk with stale
  selections. `SelectionHasStaleEntries` remains available for displaying
  a stale-selection warning. The preflight prevents partial deletion from
  invalid bookmarks; errors during the subsequent dataset deletes still
  follow the dataset's transaction behavior.
  Bookmarks taken on the loaded page round-trip exactly, and bookmarks
  taken while the mode is off behave natively.
- After a delete, `Locate` can still find the removed rows until the CDS
  change log merges (platform behavior, verified in the demo self test);
  verify deletions via `RecordCount`, a scan, or a detached copy of
  `Data` rather than `Locate`.
- Alternating-row striping (`RecNo` parity) continues across page
  boundaries instead of restarting per page.
- Export iterates the loaded page (`First` .. `Eof`) and restores the
  pre-export cursor. While virtual scrolling is active, progress callbacks
  use the loaded-page row count rather than the global `RecordCount`,
  capped by `MaxRows` when set. Reading the loaded count does not seek or
  fetch another page.
- The count stays caller-owned: a stale count misplaces the scrollbar end
  and a count below the current position leaves the cursor beyond the
  counted boundary until the next navigation; growing the count re-arms
  `HasMore` immediately.
- A cross-page seek during an active edit is refused with
  `EInvalidOperation`, matching `OpenPage`; in-page `RecNo` moves keep the
  native CDS behavior.
- Without a known count the mode is inert: `RecordCount`/`RecNo` keep
  page-local semantics and navigation stays probe-based.

For runnable examples and keyboard shortcuts, see the [main README](../README.md)
and [feature demo](../demos/features-demo/VITTIX_FULL_DEMO_README.md).
