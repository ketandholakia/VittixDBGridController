[v1.0.3] — Footer Alignment, Tests, and Installer Automation

✨ Added

- DUnitX test suite covering core grid behaviors
- Inno Setup installer for component distribution
- Build automation for installer payload generation
- GitHub release automation for tagging and publishing

🔄 Changed

- Footer summary layout now syncs with DBGrid column geometry
- Startup footer alignment now uses actual grid cell rectangles
- Installer packaging now supports Delphi 12 Athens source and package deployment

🛠 Improved

- Footer summary behavior when columns are resized
- Footer summary behavior when columns are shown or hidden
- Release workflow for publishing installer-based component builds

🐛 Fixed

- Footer summary cells not lining up with DBGrid columns at startup
- Footer alignment drift caused by coordinate-space mismatch
- ClientDataSet sorting support and related sorting behavior regressions
