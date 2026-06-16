# courier Development Guide

## Core Principles

- **Question every abstraction**: Before adding a layer, file, or indirection, ask whether it solves a current problem. If the answer is hypothetical, do not add it.
- **Refactor for net value**: A refactor must make architecture, implementation simplicity, robustness, extensibility, or test value concretely better. Moving code, renaming layers, or adding wrappers is not enough.
- **Root out helper stacking**: A pile of one-use helpers, pass-through wrappers, or accessor layers is structural debt. Inline trivial wrappers or move the whole responsibility to the owner that owns the state, commands, and formatting.
- **Simplify relentlessly**: Three similar lines are better than a premature abstraction. A single large file is better than several tiny files with unclear boundaries.
- **Reduce code by improving the model**: Prefer simpler state, data flow, control flow, and ownership over cosmetic deduplication.
- **Fewer files, clearer boundaries**: Split only when a file has a genuinely distinct responsibility. Never split for cosmetic reasons.
- **Delete, don't deprecate**: Remove unused code entirely. No backward-compatibility shims, re-exports, or "removed" comments.
- **Prefer boring code**: A straightforward conditional chain is easier to debug than clever dispatch machinery.
- **Converge UX**: Prefer one clear entry point and one consistent behavior model over overlapping commands or branchy mode-specific behavior. Wrapper commands are acceptable only when they share one resolution path and one default behavior model.

## Diagnosis and Change Discipline

- **Find the root cause before changing behavior**: Do not patch UI flow, naming, or save logic until you can name the failing layer and explain why it is responsible.
- **One failed fix narrows the hypothesis**: If the first attempted fix does not hold, reduce the hypothesis space and gather evidence. Do not stack another speculative patch on top.
- **Two failed fixes stop the patching loop**: After two failed fixes on the same issue, stop changing behavior and switch to diagnosis only.
- **Fix the right layer**: Move the fix to the code that owns the problem instead of compensating in callers, renderers, or save/load glue.
- **Stabilize workflow changes before coding**: For any change that alters a primary entry point, default action, or save flow, write a short design note first.
- **Keep experiments narrow**: Start new directions with the smallest slice that proves the workflow is worth having. Do not expand scope before the first slice shows real user value.
- **Audit the whole surface for broad refactors**: For project-wide cleanup, review affected code, tests, documentation, and user workflows before choosing changes.
- **Clarify broad refactors before coding**: When scope, compatibility, naming, ownership, user-visible behavior, or stopping criteria are unclear, inspect local evidence first, then ask focused questions only for choices that change the implementation plan.
- **Flag compensating code as design debt**: When touching a subsystem, note silent fallbacks, compatibility aliases, and swallowed internal errors as debt instead of layering more compensation on top.

## Module Boundaries

- **Split by stable responsibilities**: Extract modules around durable workflows, state ownership, external boundaries, or lifecycle boundaries. Do not split into vague `common`, `utils`, or `helpers` buckets.
- **Move whole responsibilities**: A useful extraction moves the state, operations, validation, and formatting/rendering helpers that belong together. If the old module still owns the behavior and the new file only adds glue, the split is not done.
- **Stop before glue takes over**: If an extraction mostly adds declarations, pass-through wrappers, and navigation overhead, keep the code together.
- **Do not patch boundaries with declarations**: `declare-function`, `defvar`, adapter interfaces, and protocol stubs must make real ownership explicit. If they mainly let lower-level code call upward, move the interface to the owner instead.
- **Do not call dependency internals**: Another package's private symbols are out of bounds. If Courier needs behavior hidden behind a private helper, add or request a public API and depend on that version.
- **Modularize incrementally**: Move the smallest coherent slice first, then compile and run focused tests before attempting the next extraction.

## Postmortems

The `postmortem/` directory contains design decision records. Read them before
making significant changes to request storage, save flow, UI model, or
collection behavior.

Each postmortem should record:

- background
- decision
- rationale
- alternatives considered
- known limitations

Write a postmortem when:

- changing the `.http` file format or parsing model
- changing a primary request or response workflow
- switching between non-obvious UI models
- abandoning an approach after real implementation work
- deliberately deferring a limitation that will shape later design

Write why, not what. The code already shows what changed.

Long technical specifications do not belong in `AGENTS.md`. Put detailed
formats, protocols, or migration rules in a separate document and link to them
from the relevant postmortem.

## Error Handling and Testing

- **Errors must surface, not hide**: Do not add fallback/default returns that silently swallow failures. Let errors propagate immediately.
- **Catch at the boundary, nowhere else**: Only the outermost API layer (process sentinel, top-level command handler) should catch and convert exceptions to error responses. Business logic must not `condition-case` around internal calls.
- **Robustness is not defensive programming**: Prefer clear ownership, fewer states, explicit error boundaries, and verifiable invariants over broad fallback paths.
- **Tests must fail when the code is wrong**: If deleting or breaking the function under test does not turn the test red, the test is worthless. Assert specific, distinguishable output values.
- **Test the real dispatch path for dispatch bugs**: When a bug is in completion, hooks, command routing, async callbacks, or parsing dispatch, include a test that drives the installed or public entry path.
- **Match test weight to change size**: Use the smallest test that proves the intended behavior. Do not turn comment edits, wording-only changes, or pure presentation tweaks into heavy red/green exercises.
- **Treat tests as architecture budget**: Keep tests that prove public workflows, real invariants, and meaningful edge cases. Remove or simplify tests that only lock in implementation details or duplicate another assertion.
- **No hard-coded expectations**: Use diverse inputs — multiple data sets, random values, boundary cases — so that a hard-coded return cannot satisfy all assertions.
- **Red before green for real bug fixes**: When fixing a user-visible bug or regression, first write or update a test that proves the failure path before changing the code.

## Structured Edits

- Do not transform `.http`, Org, or Elisp syntax by brittle raw string insertion when syntax boundaries matter.
- Prefer parser-backed, token-aware, or top-level-clause-aware transformations with safe fallback behavior.
- For complex constructs, prioritize semantic correctness over aggressive rewriting. Do not force a full parser into a small fix unless the change truly needs it.

## Architecture

- **Interface / implementation separation**: `courier.el` is the entry point. External consumers load `(require 'courier)`.
- **No behavioral side effects on load**: Loading a file must not alter Emacs editing behavior (no modes enabled, no hooks fired). Package-level registration side effects such as `auto-mode-alist` entries are allowed.
- **Reuse Emacs infrastructure**: Use `completing-read`, `special-mode`, `text-property-search-forward`, standard hooks, and other stock primitives.
- **Public naming**: `courier-` for all public symbols.
- **Private naming**: `courier--` for internal symbols. Never call private symbols across file boundaries without `declare-function` / `defvar` declarations.
- **External private symbols**: Never call another package's double-dash symbols.
- **Predicates**: Multi-word predicate names end in `-p`.
- **Unused args**: Prefix with `_`.
- **Prefer flat control flow**: Avoid deep `let` → `if` → `let` nesting. Use `if-let*`, `when-let*`, `pcase`, and `pcase-let`.
- **Prefer destructuring over repeated accessors**: Use `pcase-let` when repeatedly pulling related values out of the same list or plist.
- **Prefer `cl-loop` for non-trivial accumulation**: Use it instead of `dolist` plus manual accumulator state when it makes collection logic clearer.
- **Use the right error type**: `user-error` for user-caused problems; `error` for programmer bugs; `condition-case` only for recoverable boundary failures.
- **Do not wrap standard errors without semantics**: Use `user-error` or `error` directly unless a wrapper adds behavior that the builtin does not provide.
- **Prefer idiomatic primitives**: Favor direct non-nil predicates over `(not (null ...))`, and use standard sequence/string primitives instead of hand-rolled wrappers.
- **Data shape**: Prefer `let*`, `pcase-let`, alists/plists, small helpers, or table-driven mappings for short-lived context. Reserve `cl-defstruct` for stable data crossing module or lifecycle boundaries.
- **State placement**: `defvar-local` for buffer state, plain `defvar` for shared state, `defcustom` for user options with precise `:type` and `:group`. Major modes must make their state buffer-local.
- **Mode definitions**: Read-only UI buffers derive from `special-mode`; editing buffers derive from `text-mode`.
- **Rendering discipline**: Use text properties for data-bearing annotations and overlays only for ephemeral visuals. Render buffers from structured buffer-local state, not by reparsing displayed text.
- **Infrastructure boundaries**: Keep target resolution, action definition, and action presentation separate. Transient menus and other presentation integrations must not become independent business-logic systems.
- **Function design**: Keep functions short, separate pure computation from display mutation, and keep interactive commands thin.

## Version Baseline

- `courier` targets **Emacs 29.1+**.
- `transient` is an approved runtime dependency for action menus.
- curl must be available on PATH.

## MELPA Compatibility

### File headers

- First line: `;;; file.el --- Short description -*- lexical-binding: t; -*-`
  - Description must NOT contain "for Emacs" or the package name.
  - Keep the description under 60 characters.
- `;; Package-Requires:` must include `((emacs "29.1"))` and any approved
  runtime dependencies currently used by Courier, including `transient`.
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
- Avoid `with-eval-after-load` in package code unless it registers an optional integration at a clear package boundary.

## Pre-Commit Checklist (Mandatory)

Every commit must pass all of these steps.

### 1. Read the full diff

```bash
git diff HEAD
```

Read every changed line before committing.

Also scan for accidental boundary violations, especially calls to external private symbols or new declarations that patch the wrong ownership boundary.

### 2. Run all tests

```bash
emacs -batch -L . -l ert -l courier -l test/courier-test.el \
  --eval '(ert-run-tests-batch-and-exit)'
```

### 3. Byte-compile with zero warnings

```bash
emacs -batch -L . -f batch-byte-compile *.el
```

### 4. checkdoc with zero warnings

```bash
emacs -batch -L . -l courier.el --eval '(checkdoc-file "courier.el")'
```

### 5. package-lint with zero warnings

```bash
emacs -batch -L . \
  -L ~/.emacs.d/straight/repos/package-lint \
  --eval '(require (quote package))' \
  --eval '(package-initialize)' \
  --eval '(dolist (entry package--builtins) (let ((name (car entry)) (meta (cdr entry))) (unless (assq name package-alist) (push (cons name (list (package-desc-create :name name :version (append (aref meta 0) nil) :summary (aref meta 2) :reqs (aref meta 1) :kind (quote builtin)))) package-alist))))' \
  --eval '(require (quote package-lint))' \
  --eval '(with-temp-buffer (insert-file-contents "courier.el") (emacs-lisp-mode) (let ((warnings (package-lint-buffer))) (if warnings (progn (dolist (w warnings) (message "%s" w)) (kill-emacs 1)) (message "package-lint OK"))))'
```

### 6. Update tests when behavior changes

When a function's behavior changes intentionally, search all test files for existing tests of that function and update them before committing:

```bash
grep -n "function-name" test/courier-test.el
```

Update existing tests first. Add a new failing test only when the current suite does not already prove the regression or changed behavior.

### 7. Remove shortcuts and redundancy

Do not leave heuristic partial implementations, duplicated logic, or dead code introduced by the change. If part of the correct fix is deliberately deferred, document why in the relevant postmortem or design note.

## Documentation

- Any change to key bindings, defaults, or user-visible workflow must update `README.org` in the same change.
- If code and docs diverge, treat code as source of truth and fix docs immediately.
- Optimize docs for rendered reading, not source-width aesthetics. Do not rewrap unchanged Markdown or Org prose just to fit a column.
- Fix structure before line breaks: prefer clearer headings, tables, shorter bullets, or focused rewrites over churn that only changes wrapping.
