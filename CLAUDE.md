# courier Development Guide

## Core Principles

- **Question every abstraction**: Before adding a layer, file, or indirection, ask whether it solves a current problem. If the answer is hypothetical, do not add it.
- **Simplify relentlessly**: Three similar lines are better than a premature abstraction. A single large file is better than several tiny files with unclear boundaries.
- **Fewer files, clearer boundaries**: Split only when a file has a genuinely distinct responsibility. Never split for cosmetic reasons.
- **Delete, don't deprecate**: Remove unused code entirely. No backward-compatibility shims, re-exports, or "removed" comments.

## Error Handling and Testing

- **Errors must surface, not hide**: Do not add fallback/default returns that silently swallow failures. Let errors propagate immediately.
- **Catch at the boundary, nowhere else**: Only the outermost API layer (process sentinel, top-level command handler) should catch and convert exceptions to error responses. Business logic must not `condition-case` around internal calls.
- **Tests must fail when the code is wrong**: If deleting or breaking the function under test does not turn the test red, the test is worthless. Assert specific, distinguishable output values.
- **No hard-coded expectations**: Use diverse inputs — multiple data sets, random values, boundary cases — so that a hard-coded return cannot satisfy all assertions.

## Architecture

- **Interface / implementation separation**: `courier.el` is the entry point. External consumers load `(require 'courier)`.
- **No behavioral side effects on load**: Loading a file must not alter Emacs editing behavior (no modes enabled, no hooks fired). `auto-mode-alist` registration is allowed.
- **Reuse Emacs infrastructure**: Use `completing-read`, `special-mode`, `text-property-search-forward`, standard hooks, and other stock primitives.
- **Public naming**: `courier-` for all public symbols.
- **Private naming**: `courier--` for internal symbols. Never call private symbols across file boundaries without `declare-function` / `defvar` declarations.
- **Predicates**: Multi-word predicate names end in `-p`.
- **Unused args**: Prefix with `_`.
- **Prefer flat control flow**: Avoid deep `let` → `if` → `let` nesting. Use `if-let*`, `when-let*`, `pcase`, and `pcase-let`.
- **State placement**: `defvar-local` for buffer state, plain `defvar` for shared state, `defcustom` for user options. Major modes must make their state buffer-local.
- **Mode definitions**: Read-only UI buffers derive from `special-mode`; editing buffers derive from `text-mode`.
- **Rendering discipline**: Use text properties for data-bearing annotations and overlays only for ephemeral visuals.
- **Function design**: Keep functions short, separate pure computation from display mutation, and keep interactive commands thin.

## Version Baseline

- `courier` targets **Emacs 29.1+**.
- No external Emacs package dependencies for v0.1.
- curl must be available on PATH.

## MELPA Compatibility

### File headers

- First line: `;;; file.el --- Short description -*- lexical-binding: t; -*-`
  - Description must NOT contain "for Emacs" or the package name.
  - Keep the description under 60 characters.
- `;; Package-Requires:` must list `((emacs "29.1"))`.
- `;; URL:`, `;; Version:`, `;; Author:` headers are required.
- Last line: `;;; file.el ends here`

### Naming

- Public symbols use `courier-` prefix.
- Internal symbols use `courier--` prefix.
- Every `define-derived-mode` that is user-facing should have `;;;###autoload`.
- `defcustom` `:type` must be specified.

### Autoloads

- Add `;;;###autoload` to user-facing commands and modes.
- Do NOT autoload internal helpers or variables.

### checkdoc

- Every public `defun`, `defmacro`, `defcustom`, and `defvar` must have a docstring.
- Docstring first line must be a complete sentence ending in a period.
- Argument names in docstrings should be UPPERCASED.

### Common pitfalls

- `cl-lib` functions require `(require 'cl-lib)`.
- Avoid `eval-when-compile` for runtime-needed dependencies.

## Pre-Commit Checklist

### 1. Byte-compile with zero warnings

```bash
emacs -batch -L . -l courier.el -f batch-byte-compile *.el
```

### 2. Run all tests

```bash
emacs -batch -L . -l ert -l courier -l test/courier-test.el \
  --eval '(ert-run-tests-batch-and-exit)'
```

## Pre-Commit Checklist (Mandatory)

Every commit must pass all of these steps.

### 1. Read the full diff

```bash
git diff HEAD
```

Read every changed line before committing.

### 2. Run all tests

```bash
emacs -batch -L . -l ert -l courier -l test/courier-test.el \
  --eval '(ert-run-tests-batch-and-exit)'
```

### 3. Byte-compile with zero warnings

```bash
emacs -batch -L . -f batch-byte-compile *.el
```

### 4. Update tests when behavior changes

When a function's behavior changes intentionally, search all test files for existing tests of that function and update them before committing:

```bash
grep -n "function-name" test/courier-test.el
```

## Documentation

- Any change to key bindings, defaults, or user-visible workflow must update `README.org` in the same change.
- If code and docs diverge, treat code as source of truth and fix docs immediately.
