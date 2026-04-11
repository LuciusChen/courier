# Courier PRD

## Summary

Courier is an Emacs-native HTTP client built around plain-text request files.
Its storage and collection model should stay close to Bruno: local files on
disk, Git-friendly, and easy to inspect outside the app. Its interaction model
should stay close to Emacs: buffer-first, command-first, minibuffer-driven, and
compatible with existing editing workflows.

The next stage of Courier is not "become Bruno inside Emacs". The goal is to
become a serious collection-based API tool whose architecture is Bruno-like and
whose UX is unmistakably Emacs.

## Terminology

The top-level unit is `collection`.

- `collection`: one local root directory managed by Courier
- `request`: one `.http` file
- `folder`: a plain filesystem directory used for grouping requests
- `env`: one environment variable file inside the collection
- `response history`: previous responses for the same request
- `request switcher`: command for moving between different requests or envs

The term `project` is not used in the product model. A Courier collection may
live inside a Git repository, but the Courier concept itself is `collection`.

## Product Goals

- Keep requests as normal files that Git can diff, move, rename, and review.
- Make switching between requests and environments fast from the keyboard.
- Provide an Emacs-native collection overview rather than a web-style sidebar.
- Make response inspection significantly better than the current raw/pretty
  split by adding proper response views and richer rendering.
- Keep the architecture light enough that the codebase stays understandable and
  does not require a database or background index service.

## Non-Goals

- No internal database.
- No cloud sync.
- No Electron-style multi-panel app shell.
- No heavyweight collection schema with UUIDs for every entity.
- No promotion of directories into complex first-class objects.
- No Org-mode data model.
- No Bruno-style mouse-centric UI.
- No scripting engine, OAuth manager, or advanced workflow runtime in the next
  milestone set.

## Architecture Principles

- Storage model: Bruno-like.
- UX model: Emacs-native.
- Requests are the primary source of truth.
- Filesystem paths remain meaningful and stable.
- Git is the version-control layer; Courier should not hide or replace it.
- Request text remains editable without Courier-specific forms or widgets.
- Minimal metadata only where the collection needs clear boundaries or defaults.

## Collection Model

A collection is a root directory with a root marker file:

```text
my-api/
  courier.json
  requests/
    users/
      get-user.http
      create-user.http
    admin/
      delete-user.http
  env/
    local.env
    staging.env
    prod.env
```

### Required Rules

- `courier.json` marks the collection root.
- `requests/` contains `.http` files.
- `env/` contains environment files.
- Directories under `requests/` are just directories.
- Requests are still plain `.http` files with the existing Courier syntax.

### `courier.json` v0.1

The first version of `courier.json` stays intentionally small:

- `name`
- `requestsDir`
- `envDir`
- `defaultEnv`

This file defines collection boundaries and a few defaults. It is not the main
data store for requests.

## Request Model

A request is a `.http` file with:

- directives
- request line
- headers
- body

Courier should continue to treat the request text itself as authoritative.
Request method, URL, headers, and body should always round-trip through the
file. Commands may edit the file, but no hidden request state should exist.

## Environment Model

The purpose of `env` is narrow and explicit: provide different variable values
for the same request.

Typical values:

- `base_url`
- `token`
- `tenant`
- `user_id`

Rules:

- env files live under `env/`
- env names come from filenames like `local.env`, `staging.env`, `prod.env`
- collection-level `defaultEnv` may be used when no explicit env is selected
- request-level `# @var` values override env values
- env selection is buffer-local unless a broader command explicitly changes it

`env` is not collection metadata, not response history, and not a secret
manager. It is a source of variable values for request resolution.

## UX Principles

- Use `completing-read` for selection flows.
- Use normal editing buffers for request files.
- Use `special-mode` for read-only overviews and inspectors.
- Use one-command keyboard flows rather than UI panels.
- Prefer overview buffers and pickers over popup widget systems.
- Use text properties and standard Emacs affordances before inventing custom UI
  layers.

## Reference Direction

### What Courier should learn from Bruno

- collection as filesystem root
- file-based request storage
- Git-native organization
- environments as part of the collection
- low-friction movement between requests

### What Courier should not copy from Bruno

- Electron layout
- heavy UI state model
- mouse-driven sidebars and tabs
- form-first editing

### What Courier can borrow from Verb

- plain-text request editing sensibility
- request preview ideas
- local implementation ideas for parsing and request handling

### What Courier should not borrow from Verb

- Org tree hierarchy as the primary data model
- request inheritance driven by headings
- Org-centric request management

## Core User Flows

### 1. Open a collection

- user opens any request inside a collection
- Courier finds `courier.json`
- request and env discovery are bounded by that collection root

### 2. Switch request

- user invokes one jump command
- minibuffer shows grouped candidates
- request candidates open files
- environment candidates switch env

### 3. Edit method quickly

- user invokes a method command
- minibuffer offers allowed methods
- Courier edits the request line directly

### 4. Send and inspect response

- user sends request
- response buffer centers on one response at a time, with a header-line view
  control for Response, Headers, Timeline, and Tests
- user changes response view as needed
- response history remains available for the same request

### 5. Create a new request

- user creates a new request draft without choosing a path first
- Courier gives it an `Untitled N` name and opens it as an unsaved buffer
- the draft request line uses a configurable default method, with `GET` as the default
- on first save, Courier asks which collection should own the request
- if the target directory is not a collection, Courier offers to create one
- the request is then saved into that collection's `requestsDir`

### 6. Work from overview

- user opens collection overview
- overview lists requests in an Emacs-native format
- user opens, previews, sends, filters, and switches env from there

## Current Baseline

The current baseline already includes:

- single-file implementation in `courier.el`
- plain `.http` request parsing
- collection root marker support via `courier.json`
- collection-bounded env discovery with `envDir` and `defaultEnv`
- per-buffer env switching based on `.env` files
- response history for repeated sends of the same request
- grouped request and environment picker
- unsaved request drafts with first-save collection placement
- collection overview
- response view system with dedicated body viewer
- basic response rendering, including image-aware views

The current baseline does not yet include:

- polished response presentation
- broader collection management flows beyond requests and folders
- stronger cleanup against compatibility shims and hidden parse fallbacks

## Roadmap

### Phase 1: Collection Root

Deliver:

- collection root discovery via `courier.json`
- bounded request discovery
- bounded env discovery

Acceptance:

- Courier stops scanning past the collection boundary
- request and env lookup are deterministic inside one collection

### Phase 2: Environment Rules

Deliver:

- `env/` layout support
- env naming rules
- `defaultEnv`
- env resolution tests
- docs that explain env precisely

Acceptance:

- env behavior is predictable and documented
- request-level vars override env vars correctly

### Phase 3: Unified Picker

Deliver:

- one command that shows `Requests` and `Environments`
- grouped minibuffer display
- different ordering rules per group
- request selection opens files
- env selection switches current env

Acceptance:

- different requests and envs are reachable from one picker
- grouped display is clear enough to use without memorizing paths

### Phase 4: Request Editing Ergonomics

Deliver:

- method selection command
- optional GET/POST toggle
- request-line edits as the only source of truth

Acceptance:

- changing method no longer requires manually editing the first line

### Phase 5: Collection Overview

Deliver:

- overview buffer for one collection
- request listing
- filtering and search
- open, send, preview, env actions

Acceptance:

- overview becomes the main entry point for collection-scale work

### Phase 6: Response Views

Deliver:

- explicit response view system
- target views:
  - `auto`
  - `json`
  - `html`
  - `xml`
  - `javascript`
  - `raw`
  - `hex`
  - `base64`
  - `image`

Acceptance:

- response viewing is no longer limited to pretty/raw
- common content types have an obvious best view

### Phase 7: Response UI Upgrade

Deliver:

- cleaner header line
- a clickable header-line current-view control for Response, Headers, Timeline, and Tests
- a collapsed-by-default Timeline history list
- expandable Timeline detail sections for Request, Response, and Network Logs
- syntax highlighting through suitable major modes
- image rendering support
- better body viewer behavior

Acceptance:

- response buffers are pleasant enough for repeated daily use
- timeline inspection is useful without leaving the response buffer
- timeline defaults to history browsing instead of forcing an always-expanded inspector

### Phase 8: Collection Management

Deliver:

- new request
- rename request
- move request
- new folder

Acceptance:

- common collection maintenance actions happen inside Courier without adding a
  separate metadata system

## Quality Gates

Every milestone must include:

- PRD updates when scope or terminology changes
- README updates for user-visible behavior
- ERT coverage for the new behavior
- zero-warning byte compilation
- full test pass before commit

## Immediate Next Steps

The next implementation work should follow this order:

1. continue polishing response presentation and body viewers
2. add lightweight collection management commands
3. refine overview interactions and filtering as usage feedback arrives
4. revisit image-heavy and large-body workflows after real usage
