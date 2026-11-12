# 🖥️ GVimCLR — ClojureCLR + Avalonia Vim Clone

A lightweight, modern **gvim clone** written in **ClojureCLR** and **Avalonia UI** running natively on Linux (.NET 10).

It combines the power of Vim modal editing with a sleek graphical interface featuring Catppuccin Mocha styling, line numbering gutters, quick-action toolbars, Powerline-style statuslines, and a complete `:` command-line engine.

---

## ✨ Features

* **⌨️ Full Vim Modal Editing:**
  * **NORMAL Mode:** Navigation (`h`/`j`/`k`/`l`, `w`/`b`, `0`/`$`, `gg`/`G`), line editing (`dd`, `yy`, `p`, `P`), character operations (`x`, `~`), undo/redo (`u`, `Ctrl-R`).
  * **INSERT Mode:** Unhindered typing at cursor, `Esc` returns to Normal mode.
  * **COMMAND Mode (`:`):** Full command-line engine supporting `:w`, `:w <file>`, `:e <file>`, `:q`, `:q!`, `:wq`, `:<line>`, `:%s/old/new/g`, `:help`.
  * **SEARCH Mode (`/`):** Instant search highlighting, `n` (next match), `N` (previous match).
* **🔢 Line Numbers Gutter:**
  * Dedicated left-side margin with line numbers (`1`, `2`, `3`...) synchronized in real-time with buffer length.
* **⚡ Powerline-Style Statusline:**
  * Color-coded mode badges: `NORMAL` (Green), `INSERT` (Blue), `COMMAND` (Mauve), `SEARCH` (Peach).
  * File name, modification dirty flag `[+]`, cursor position `ln X, col Y`, file percentage, and line count.
* **🛠️ Graphical Toolbar:**
  * One-click buttons for common actions: New, Open, Save, Undo, Redo, Cut, Copy, Paste.
* **🎨 Modern Catppuccin Theme:**
  * Slate dark background (`#1e1e2e`), monospaced typography, and syntax-friendly contrast.

---

## ⌨️ Vim Keybindings Reference

### Normal Mode Navigation & Editing

| Key | Action |
|:---:|:---|
| **`h`** / **`Left`** | Move cursor left |
| **`l`** / **`Right`** | Move cursor right |
| **`j`** / **`Down`** | Move cursor down |
| **`k`** / **`Up`** | Move cursor up |
| **`w`** | Jump to start of next word |
| **`b`** | Jump to start of previous word |
| **`0`** | Jump to beginning of line |
| **`$`** (Shift+4) | Jump to end of line |
| **`gg`** | Jump to first line of buffer |
| **`G`** (Shift+g) | Jump to last line of buffer |
| **`x`** | Delete character under cursor |
| **`dd`** | Delete (cut) current line to clipboard |
| **`yy`** | Yank (copy) current line to clipboard |
| **`D`** (Shift+d) | Delete from cursor to end of line |
| **`Y`** (Shift+y) | Yank from cursor to end of line |
| **`C`** (Shift+c) | Change to end of line (delete to end and enter Insert mode) |
| **`S`** (Shift+s) | Change whole line (delete line and enter Insert mode) |
| **`s`** | Substitute character (delete char and enter Insert mode) |
| **`J`** (Shift+j) | Join current line with next line |
| **`p`** | Paste clipboard / buffer below current line |
| **`P`** | Paste clipboard / buffer above current line |
| **`u`** | Undo last change |
| **`Ctrl-R`** | Redo last undone change |
| **`~`** | Toggle case of character under cursor |
| **`v`** | Enter Character-wise Visual Mode (` VISUAL `) |
| **`V`** (Shift+v) | Enter Line-wise Visual Mode (` V-LINE `) |
| **`Ctrl-V`** | Enter Block-wise Visual Mode (` V-BLOCK `) |
| **`Ctrl-[`** / **`Esc`** | Exit any mode back to Normal mode |
| **`i`** | Enter Insert mode before cursor |
| **`I`** | Enter Insert mode at beginning of line |
| **`a`** | Enter Insert mode after cursor |
| **`A`** | Enter Insert mode at end of line |
| **`o`** | Open new line below and enter Insert mode |
| **`O`** | Open new line above and enter Insert mode |
| **`:`** | Enter Command-line mode |
| **`/`** | Enter Search mode |
| **`n`** | Jump to next search match |
| **`N`** | Jump to previous search match |

### Command-Line Mode (`:`)

| Command | Action |
|:---|:---|
| **`:w`** | Save buffer to current file |
| **`:w <filename>`** | Save buffer as `<filename>` |
| **`:e <filename>`** | Open and load `<filename>` |
| **`:q`** | Quit (fails if buffer has unsaved changes) |
| **`:q!`** | Force quit (discards unsaved changes) |
| **`:wq`** / **`:x`** | Save buffer and quit |
| **`:<number>`** | Jump directly to line number `<number>` |
| **`:%s/old/new/g`** | Global search and replace `old` with `new` |
| **`:s/old/new`** | Replace first occurrence of `old` with `new` |
| **`:help`** | Display keybinding help summary |

---

## 🚀 Running GVimCLR

### Quick Launch
```bash
cd /tmp/gvimclr
./run.sh
```

Or via `dotnet`:

```bash
cd /tmp/gvimclr
dotnet run
```

---

## 🛠️ Building & Publishing

Compile from source:

```bash
cd /tmp/gvimclr
dotnet build
```

Publish native Linux release binary:

```bash
dotnet publish -c Release --self-contained false -o dist
./dist/gvimclr
```

---

## 📁 Architecture & File Layout

```
/tmp/gvimclr/
├── gvimclr.csproj           # .NET 10 project file (ClojureCLR + Avalonia packages)
├── Program.cs               # Avalonia bootstrap & C# UI interop helper (tunneling keys, caret)
├── run.sh                   # Launch script with Wayland/X11 environment resolution
├── README.md                # Project documentation
└── src/
    └── clojure/
        └── gvim/
            └── core.clj     # Full Vim modal state machine, editor UI & commands in ClojureCLR
```
