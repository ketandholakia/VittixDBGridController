# Contributing to VittixDBGridController

First of all, thank you for considering contributing to **VittixDBGridController** 🙏  
All contributions are welcome — bug reports, feature requests, documentation, and code improvements.

This document explains how to contribute in a clean, consistent, and maintainable way.

---

## 📌 Project Philosophy

VittixDBGridController follows these principles:

- ✅ **Composition over inheritance** (no subclassing `TDBGrid`)
- ✅ **Non-intrusive design** (plug-and-play controller)
- ✅ **Clean separation of concerns**
  - Controller
  - Sorting engine
  - Filtering engine
  - Aggregation engine
  - UI helpers
  - Persistence
- ✅ **Backward compatibility**
- ✅ **Readable, maintainable Object Pascal**

Please keep these principles in mind when contributing.

---

## 🧰 Supported Delphi Versions

Contributions should compile and work on:

- Delphi **10.4 Sydney**
- Delphi **11 Alexandria**
- Delphi **12 Athens** (recommended)

If you use newer RTL features, please note it in your PR description.

---

## 🗂 Repository Structure

Please respect the existing structure:

```text
source/
├─ core/          Core controller & column metadata
├─ sorting/       Sorting engine
├─ filtering/     Filtering engine & popup UI
├─ aggregation/   Aggregation engine
├─ ui/            UI helpers (footer, editors, column chooser)
├─ persistence/   JSON state storage
└─ resources/     .res and visual assets
```

**Do not**:
- Move files arbitrarily
- Merge unrelated responsibilities into one unit
- Introduce circular unit dependencies

---

## 🧪 Demos

If your change affects behavior or UI, please update or add a demo:

- `demos/BasicDemo` → minimal usage
- `demos/AdvancedDemo` → all features
- `demos/NorthwindDemo` → real-world database usage

Demos must:
- Compile without warnings
- Run without additional setup (except SQLite DB already included)

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
- Add comments where intent is not obvious

### Memory Management
- Always free owned objects
- Prefer `try/finally`
- Avoid hidden ownership

---

## 🧠 Controller Rules (Important)

- ❌ Do NOT access private fields of `TVittixDBGridController`
- ✅ Use public methods (`Refresh`, `ApplyState`, etc.)
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
   - No .dcu, .exe, .identcache files included
5. Open a Pull Request

---

## 🚦 Versioning

This project follows Semantic Versioning:

- **MAJOR** – Breaking changes
- **MINOR** – New features (backward compatible)
- **PATCH** – Bug fixes

---

## 📜 License

By contributing, you agree that your contributions will be licensed under the same license as this project.

---

## ❤️ Thank You

Your time and effort are greatly appreciated.

If you’re unsure about anything, feel free to open an issue or start a discussion.

Happy coding 🚀
— VittixDBGridController Team