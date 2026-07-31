# Vittix DBGrid Feature Demo

This folder contains the full demo form for the Vittix DBGrid component suite.

The demo uses a `TClientDataSet` with 50 sample records and does not require an external database.

## What it shows

- Multi-column sorting
- Column filters with popup operators, recent values, distinct-value mode, `Not Between`, `Enter` commit, `Esc` cancel, and history clearing
- Global search across all columns
- Footer aggregations with clear and copy actions plus keyboard shortcuts
- Column chooser search, drag reordering, reset, and keyboard shortcuts
- JSON layout persistence for column order, widths, visibility, sort state, aggregations, footer visibility, and alternate row color
- Export dialog with CSV, TSV, Excel, HTML, XML, JSON, clipboard, and text output
- Memo editing, date/time editing, copy-cell/copy-row actions, alternating rows, and live status updates

## New in this update

- The demo now uses the controller's JSON layout persistence instead of the older hand-rolled INI snapshot
- Saved demo state lives in `features-demo-state` next to the executable, with a temp fallback if that folder cannot be created
- The column chooser and footer popup shortcuts now match the current component behavior
- The stale export-selection menu item was removed from the demo

## Demo files

- `VittixDBGridFullDemo.dpr`
- `VittixDBGridForm.pas`
- `VittixDBGridForm.dfm`

## State files

- `layout.json`
- `chooser.ini`
- `filter.ini`
- `export.ini`

## How to use the demo

1. Start the project from `VittixDBGridFullDemo.dpr`.
2. Click column headers to sort.
3. Right-click a header to open the filter popup.
4. Open the column chooser and try the search box and shortcuts.
5. Right-click footer cells to clear or copy aggregations.
6. Use Save Layout and Load Layout to persist the current arrangement.
7. Open Export Data to test the export formats, including text and clipboard.

## Shortcut summary

Column chooser:

- `Ctrl+F` focus search
- `Esc` clear search
- `Ctrl+A` select all
- `Ctrl+N` select none
- `Ctrl+R` reset layout
- `Ctrl+Plus` increase width
- `Ctrl+Minus` decrease width
- `Ctrl+Up` move selected column up
- `Ctrl+Down` move selected column down

Filter popup:

- `Enter` apply changes
- `Esc` cancel
- `Ctrl+Shift+H` clear history
- `Not Between` is available in the operator list
- Distinct-value mode is available when the popup is configured for it

Footer popup:

- `Del` clear the current aggregation
- `Ctrl+Del` clear all aggregations
- `Ctrl+C` copy the current aggregation
- `Ctrl+Shift+C` copy all aggregations
- `Ctrl+Shift+F` copy the footer summary

## Sample data

The demo loads 50 sample records with 15 fields:

- `ID`
- `CompanyName`
- `ContactName`
- `Email`
- `Phone`
- `Country`
- `City`
- `OrderDate`
- `TotalAmount`
- `Quantity`
- `Status`
- `Notes`
- `Discount`
- `ShippingDate`
- `PaymentMethod`

## Layout persistence

The demo stores the following state under `features-demo-state`:

- Grid layout JSON
- Column chooser state
- Filter history
- Export dialog state

If the folder cannot be created next to the executable, the demo falls back to a temp location.

## Notes

- The demo is intended for Delphi VCL projects.
- `Load Layout` restores the saved grid arrangement, including footer visibility and alternate row color.
- `Save Layout` writes the current grid state to JSON.
