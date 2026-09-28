# Contributing to VittixDBGridController

First of all, thank you for considering contributing to **VittixDBGridController** 🙏
All contributions are welcome — bug reports, feature requests, documentation, and code improvements.

This document explains how to contribute in a clean, consistent, and maintainable way.

---

## 📌 Project Philosophy

VittixDBGridController follows these principles:

- ✅ **Controller-based architecture** — `TVittixDBGrid` subclasses `TDBGrid` and delegates all engine work (sorting, filtering, aggregation, persistence) to a `TVittixDBGridController` and its logic-only engines
- ✅ **Clean separation of concerns**
  - Grid (`Vittix.DBGrid.pas`) — visual control, forwards input/draw events to the controller
  - Controller (`Vittix.DBGrid.Controller.pas`) — lifecycle, hooking, engine ownership
  - Engines — sort, filter, aggregation (pure logic, no UI)
  - UI helpers — footer panel, editors, column chooser, filter popup
  - Persistence — JSON layout/state storage
- ✅ **Backward compatibility**
- ✅ **Readable, maintainable Object Pascal**

Please keep these principles in mind when contributing.

---

## 🧰 Supported Delphi Versions

The supported and CI-tested toolchain is:

- Delphi **12 Athens** (Win32) — this is what the packages, the test suite and the installer are built with

The source avoids RTL features newer than Delphi 10.3 where practical, but
only 12 is actively tested. If a change needs a newer RTL feature, please
note it in your PR description.

---

## 🗂 Repository Structure

Please respect the existing structure:

```text
source/           All runtime units (flat, unit-named by area)
├─ Vittix.DBGrid.pas                  The grid control
├─ Vittix.DBGrid.Controller.pas       Controller + engine lifecycle
├─ Vittix.DBGrid.ColumnInfo.pas       Column metadata (single source of truth)
├─ Vittix.DBGrid.Sort.Engine.pas      Sorting engine
├─ Vittix.DBGrid.Filter.Engine.pas    Filtering engine (logic only)
├─ Vittix.DBGrid.Filter.Popup.pas     Filter popup UI
├─ Vittix.DBGrid.Aggregation.Engine.pas
├─ Vittix.DBGrid.FooterPanel.pas      Footer rendering
├─ Vittix.DBGrid.Export.Engine.pas    Export engine
├─ Vittix.DBGrid.Export.Dialog.pas    Export dialog
├─ Vittix.DBGrid.Layout.pas           JSON layout storage
└─ ...
packages/         Runtime + design-time .dpk/.dproj
tests/            DUnitX test suite (VittixDBGridTests.dpr, build-tests.bat)
demos/            features-demo — the complete feature demo
installer/        Inno Setup script + payload build scripts
docs/             Roadmap and docs index
```

**Do not**:

- Move files arbitrarily
- Merge unrelated responsibilities into one unit
- Introduce circular unit dependencies

---

## 🧪 Tests and Demos

Behavioral changes need regression coverage. The test suite is DUnitX-based:

```bat
cd tests
build-tests.bat                      rem set VITTIX_DELPHI_ROOT to override the Delphi path
VittixDBGridTests.exe
```

Run the suite before submitting; it must be green (the clipboard tests can
fail spuriously on machines where another process holds the clipboard —
rerun if only those fail).

If your change affects behavior or UI, please also update the demo in
`demos/features-demo`. Demos must compile with a plain Delphi installation
(no third-party/commercial units) and run without additional setup.

---

## 🐞 Bug Reports

When reporting a bug, please include:

1. Delphi version
2. Windows version
3. Minimal code sample
4. Expected behavior
5. Actual behavior
6. Screenshot (if UI-related)

Open an issue using the **Bug Report** template if available.

---

## 💡 Feature Requests

Feature requests are welcome!

Please describe:
- What problem it solves
- Why it belongs in the controller (not application code)
- Any breaking changes (if applicable)

Large features should be discussed **before** submitting a PR.

---

## 🧾 Coding Guidelines

### Naming
- Units: `Vittix.DBGrid.<Area>.<Name>.pas`
- Classes: `TVittixDBGrid...`
- Avoid generic names like `Utils`, `Helpers`

### Style
- Use `begin/end` blocks consistently
- Prefer clarity over cleverness
- Avoid deeply nested logic
- Comment the **why**, not the change: commit history and CHANGELOG.md carry
  the "what changed"; source comments explain constraints and intent

### Memory Management
- Always free owned objects
- Prefer `try/finally`
- Avoid hidden ownership
- Register `FreeNotification` for components referenced across owners

---

## 🧠 Controller Rules (Important)

- ❌ Do NOT access private fields of `TVittixDBGridController`
- ✅ Use public methods (`Refresh`, `ApplyState`, `SetColumnAggregation`, etc.)
- ❌ Do NOT change DBGrid internals directly unless unavoidable
- ✅ Use `ColumnInfo` as the single source of truth

---

## 📦 Packages & Design-Time Code

- Runtime logic → `source/`
- Design-time registration → `packages/`
- Keep design-time units minimal
- No runtime logic inside registration units

---

## 🔁 Pull Request Process

1. Fork the repository
2. Create a feature branch:
   ```bash
   git checkout -b feature/my-feature
   ```
3. Commit with meaningful messages:
   ```text
   Add JSON column visibility persistence
   ```
4. Ensure:
   - Project compiles
   - The DUnitX suite passes
   - No .dcu, .exe, .identcache files included
5. Open a Pull Request

---

## 🚦 Versioning

This project follows Semantic Versioning:

- **MAJOR** – Breaking changes
- **MINOR** – New features (backward compatible)
- **PATCH** – Bug fixes

Releases are tracked in [CHANGELOG.md](CHANGELOG.md).

---

## 📜 License

By contributing, you agree that your contributions will be licensed under the same license as this project.

---

## ❤️ Thank You

Your time and effort are greatly appreciated.

If you’re unsure about anything, feel free to open an issue or start a discussion.

Happy coding 🚀
— VittixDBGridController Team
