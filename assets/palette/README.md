`TVittixDBGrid.bmp` is the 24×24, 24-bit component palette icon. The
`TVITTIXDBGRID` resource name must match the registered component class.
The bottom-left magenta pixel is Delphi's transparency color.

To regenerate the resource from this directory with Delphi's tools on `PATH`:

```powershell
python generate.py
brcc32 Vittix.DBGrid.Reg.rc
Move-Item Vittix.DBGrid.Reg.res Vittix.DBGrid.Reg.dcr -Force
```

The design package links the `.dcr` directly.
