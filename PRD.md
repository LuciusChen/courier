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
- No OAuth manager or advanced workflow runtime beyond request-local Elisp
  hooks in the next milestone set.

## Architecture Principles

- Storage model: Bruno-like.
- UX model: Emacs-native.
- Requests are the primary source of truth.
- Filesystem paths remain meaningful and stable.
- Git is the version-control layer; Courier should not hide or replace it.
- Request text remains editable without Courier-specific forms or widgets.
- Minimal metadata only where the collection needs clear boundaries or defaults.

## Collection Model

Courier has a home directory for package-managed persisted content. Collections
live under a dedicated `collections/` directory inside that home:

```text
~/courier/
  collections/
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
  specs/
  state/
```

`specs/` stores imported API specifications. The current importer target is
OpenAPI JSON or YAML; imported specs are copied into `specs/<collection>/`.

### Required Rules

- Courier home is not itself a collection.
- `collections/` stores collection roots.
- `courier.json` marks the collection root.
- `requests/` contains `.http` files.
- `env/` contains environment files.
- Directories under `requests/` are just directories.
- Requests are still plain `.http` files with the existing Courier syntax.

### `courier.json`

`courier.json` still marks the collection root, but it also carries shared
defaults:

- `name`
- `requestsDir`
- `envDir`
- `defaultEnv`
- `defaults.vars`
- `defaults.headers`
- `defaults.auth`
- `defaults.timeout`
- `defaults.follow_redirects`

Collection roots may also contain nested `courier.json` files under
`requests/`. Those nested files are folder defaults, not new collection roots.

Merge order:

- collection root defaults
- nested folder defaults
- request front matter

This keeps requests as the primary data store while still allowing shared
configuration without Org-style inheritance.

## Import Scope

Current importer targets:

- OpenAPI JSON
- OpenAPI YAML

OpenAPI import rules:

- copy the source spec into `~/courier/specs/<collection>/openapi.<ext>`
- generate Courier requests under the target collection
- map `servers[0].url` to collection default `base_url`
- map path + method to request files and URLs
- map supported auth schemes: bearer, basic, apiKey, and basic oauth2
  client-credentials
- map supported body types: json, xml, text, form-urlencoded, multipart, and
  binary
- write unsupported mappings to `specs/<collection>/import-report.org`
- YAML import is normalized through system Ruby/Psych and then handled by the
  same internal OpenAPI importer

Tool-specific importers remain intentionally deferred.

## Request Model

A request is a `.http` file with:

- optional TOML front matter
- request line
- headers
- body

Courier should continue to treat the request text itself as authoritative.
Request method, URL, headers, and body should always round-trip through the
file. Commands may edit the file, but no hidden request state should exist.
Query params remain part of the URL text itself; structured params views may
edit them, but saving must write them back into the URL rather than storing a
second truth.
Body type, auth metadata, request vars, tests, and scripts belong in front
matter.
Request-side scripting is allowed, but only through explicit text blocks in the
request file; Courier should not invent hidden UI-only state for scripts.

### Request Format

The request-file format is defined in [docs/http-format-v1.md](./docs/http-format-v1.md):

- keep `.http` as the extension
- move Courier metadata into TOML front matter
- keep one raw HTTP request block for method, URL, headers, and body
- make `body.type` an explicit metadata field with real send-time semantics
- keep query params attached to the URL instead of creating a second stored
  params representation

This direction is meant to make request-side views source-backed and
round-trippable without hidden state.

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
- variable precedence is:
  - collection defaults
  - folder defaults
  - selected env vars
  - runtime vars
  - request `[vars]`
  - request `[vars.pre_request]`
- env selection is buffer-local unless a broader command explicitly changes it

`env` is not collection metadata, not response history, and not a secret
manager. It is a source of variable values for request resolution.

## UX Principles

- Use `completing-read` for selection flows.
- Use normal editing buffers for request files.
- Use `special-mode` for read-only inspectors.
- Use one-command keyboard flows rather than UI panels.
- Prefer minibuffer pickers over popup widget systems.
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
- request candidates are grouped by collection
- request candidates keep the file-derived label as the primary text
- request candidates open files
- request candidates are derived from filesystem paths, not parsed request contents

### 3. Open environment file

- user invokes a dedicated env-file command
- minibuffer shows environment files from known collections
- env candidates are grouped by collection
- env candidates keep the env name as the primary text
- env file path appears in the annotation
- selecting an env candidate opens the `.env` file for editing

### 4. Switch active environment

- user invokes request-local env switching
- Courier prompts with env names from the current request collection
- selecting a name switches the active env for the current request buffer only

### 5. Edit method quickly

- user invokes a method command
- minibuffer offers allowed methods
- Courier edits the request line directly

### 6. Send and inspect response

- user sends request
- response buffer centers on one response at a time, with the header line
  showing `View: Response`, `View: Headers`, `View: Timeline`, or
  `View: Tests`, followed by the status summary
- user changes response view as needed through a jump command
- response history remains available for the same request
- `C-c ?` opens a context-aware action menu in request and response buffers

### 7. Create a new request

- user creates a new request draft without choosing a path first
- Courier gives it an `Untitled N` name and opens it as an unsaved buffer
- the draft request line uses a configurable default method, with `GET` as the default
- the draft name remains metadata and buffer identity; the editor body starts
  at the request line instead of showing the name as editable first-line content
- on first save, Courier asks which collection should own the request
- Courier stores and discovers collections under `courier-home-directory/collections/`
- if the chosen collection name does not exist yet, Courier creates it under
  that home-managed collections directory
- Courier then asks for the request filename explicitly and appends `.http`
  automatically unless the user already typed it
- the request is then saved into that collection's `requestsDir`
- request editing stays text-first; the header line shows the current section,
  such as `Section: Body: JSON`, while `C-c C-j` switches to any request
  section
- transient remains for actions such as send, preview, save, and env
  switching, not for primary content editing
- request editing stays text-first:
  - complex typed changes use explicit commands such as body type, auth type,
    env switching, attach file, and response-var scaffolding
  - `[[vars.post_response]]` gets one scaffold command because its TOML shape
    is non-obvious
  - `tests` stays plain text and uses completion snippets instead of a
    separate insert workflow
- query params may be edited either directly in the URL or through a dedicated
  key/value editor; once edited structurally, Courier normalizes them into
  request params instead of keeping a duplicated URL-query truth
- request body handling is type-aware rather than implicit:
  - `json`, `xml`, `text`, `form-urlencoded`, `multipart`, `binary`, and
    `none` are first-class runtime types
  - the body type is stored in front matter and applied during request
    resolution
  - missing body-type-specific `Content-Type` headers are filled in only when
    the request did not already declare one
- request auth handling is also type-aware:
  - `none`, `bearer`, `basic`, `header`, `api_key`, and `oauth2` are already first-class
    runtime types
  - `oauth2` currently means `client_credentials` and performs a token fetch
    before the main request
  - the auth type remains stored in front matter
  - the request editor exposes auth type as an explicit setting instead of
    making users infer it from free-form text

## Current Baseline

The current baseline already includes:

- single-file implementation in `courier.el`
- plain `.http` request parsing
- collection root marker support via `courier.json`
- collection-bounded env discovery with `envDir` and `defaultEnv`
- per-buffer env switching based on `.env` files
- response history for repeated sends of the same request
- split request and env pickers with collection-grouped display
- unsaved request drafts with first-save collection placement
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

### Phase 3: Split Pickers

Deliver:

- one request picker that opens requests from anywhere
- one env picker that opens env files from anywhere
- collection-grouped minibuffer display
- request and env actions use distinct commands with distinct semantics
- `xref-find-definitions` in request/env buffers jumps to the effective
  source-backed variable definition

Acceptance:

- requests are reachable globally without path memorization
- env files are reachable globally without overloading request open semantics
- grouped display is clear enough to use without memorizing paths

### Phase 4: Request Editing Ergonomics

Deliver:

- method selection command
- optional GET/POST toggle
- request-line edits as the only source of truth

Acceptance:

- changing method no longer requires manually editing the first line

### Phase 5: Response Views

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
  - `document`

Acceptance:

- response viewing is no longer limited to pretty/raw
- common content types have an obvious best view

### Phase 7: Response UI Upgrade

Deliver:

- cleaner header line
- a header-line current-view indicator for Response, Headers, Timeline, and Tests,
  switched through a jump command
- a collapsed-by-default Timeline list
- expandable Timeline detail sections for Request, Response, and Network Logs
- syntax highlighting through suitable major modes
- image rendering support
- PDF/document body support
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
3. revisit image-heavy and large-body workflows after real usage
