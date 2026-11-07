# Vittix DBGrid — paged dataset demo (global scrollbar)

A runnable demo of `TVittixPagedDataSet` with a known total row count
(`TotalCount`), `VirtualScroll`, the `TVittixPagedDataSetBar` page bar,
and a `TVittixGlobalNavigator` for row navigation.

## Run it

Open `VittixPagedDemo.dproj` (Delphi 12, Win32) and press F9, or build and
start from the command line:

```bat
cd demos\paged-demo
msbuild VittixPagedDemo.dproj /p:Config=Debug /p:Platform=Win32
VittixPagedDemo.exe
```

The demo simulates a source of 1,234 rows (adjustable) and loads only the
current page of 100 rows; everything else stays on the simulated server.

## What to look at

- **Global scrollbar.** With *Virtual scroll* checked, the grid's vertical
  scrollbar spans all 1,234 rows. Jump to row 700 with the thumb: the
  dataset loads page 7 and positions on global row 700 while keeping one
  page in memory. Uncheck *Virtual scroll* to see the page-local scrollbar
  and the bar caption drop from "Page X of 13" to "Page X".
- **Navigation bar.** First / Previous / Next / Last with the
  "Page X of 13" caption; Last jumps straight to the partial page 13
  (rows 1201–1234).
- **Row navigator.** Its First / Last buttons reach rows 1 and 1,234
  even from an interior page. Prior / Next move within the loaded page.
- **Selection across pages.** Ctrl+Click a few rows, change pages, and the
  status line marks the selection *STALE*. *Delete selection* refuses with
  an error rather than deleting rows it can no longer identify (and the
  grid's Ctrl+Delete refuses too). The button uses
  `Grid.DeleteSelectedRowsSafely`; direct VCL `SelectedRows.Delete` can
  partially delete a mixed valid/stale selection. `SelectedRows.Refresh`
  prunes stale entries.
- **Changing the source.** Editing *Source rows* (or toggling virtual
  scroll) reloads from page 1 and resizes the scrollbar to the new total.
  The count is caller-owned: if you delete rows, lower the source count so
  the scrollbar end stays truthful.

## Verification

`VittixPagedDemoSelfTest.exe` (console companion, `Release`/`Debug`
Win32) creates the same form, makes it visible, and programmatically
checks the global scrollbar range and position, bar page changes, stale
selection refusal and pruning, exact valid-selection deletion, and the
virtual-scroll toggle — then exits with the number of failed checks and
writes `selftest-results.txt`:

```bat
VittixPagedDemoSelfTest.exe   ; echo %ERRORLEVEL%
```

## What remains page-local

- `TDataSet.First`/`Last` are non-virtual VCL methods: they stay on the
  loaded page. The grid's endpoint navigation and the demo's
  `TVittixGlobalNavigator` First/Last buttons reach the global endpoints.
  A stock `TDBNavigator` and direct calls remain page-local. The
  navigator's Prior/Next buttons remain page-local unless
  `AutoPageOnScroll` is enabled.
- Grid filtering, client-side sort, aggregates, and export operate on the
  loaded page only. A client `Filtered` also suspends virtual mode until
  it clears. Export progress uses the loaded row count as its total (or
  `MaxRows` when lower), rather than the global source count.
- `RecordCount` is the global total while virtual mode is on; the loaded
  row count is not readable through it.
- After a `Locate`, deleted rows can still be found until the CDS change
  log merges (platform behavior); the demo's self test therefore verifies
  deletions against a detached copy of the view packet.
