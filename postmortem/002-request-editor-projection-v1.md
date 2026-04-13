# 002 Request Editor Projection v1

## Background

Courier already settled the v1 request file shape:

- TOML front matter for Courier metadata
- one raw HTTP request block for method, URL, headers, and body

That file format decision solved storage and round-tripping, but the request
editor still leaked implementation details in a few places:

1. Editing query params structurally could make the request line show only the
   base URL even though the effective request still contained query params.
2. The Tests section rendered as a TOML fragment instead of the plain-text
   assertion list described in the PRD.
3. Vars and Scripts stayed source-backed, but their phase boundaries were hard
   to scan in the section view.
4. A richer UI risked sliding into form-editor CRUD commands that work against
   Courier's text-first model.

## Decision

Keep the on-disk request format unchanged and change only the editor
projection:

- the request line in section view shows the effective URL
- Tests are edited as one assertion per line in the section view
- Vars and Scripts remain single top-level sections, but add comment headings
  for request / pre-request / post-response groupings
- typed commands remain limited to structural changes such as body type, auth
  type, and response-var scaffolding

Courier still serializes the same front matter:

- query params stay attached to the URL on disk
- tests still save as `tests = [...]`
- vars and scripts still save as their existing TOML tables

## Rationale

- This preserves the single source of truth for query params and avoids
  inventing a second stored params representation.
- The request identity block should reflect what Courier will actually send,
  not the editor's temporary model split between `:url` and `:params`.
- Tests are easier to scan and edit as assertion lines than as a TOML array.
- Keeping Vars and Scripts as single sections avoids fragmenting the jump menu
  while still making lifecycle phases visible.
- Avoiding add/remove/move CRUD commands keeps Courier aligned with direct text
  editing instead of turning it into a form editor.

## Alternatives Considered

### Add a stored `[params]` table

Rejected.

This would introduce a second request truth and break the existing v1 file
format direction.

### Split Vars and Scripts into more top-level request sections

Rejected for now.

This makes the jump model more fragmented without changing the underlying file
format or improving the common editing path enough to justify the extra
surface area.

### Add multipart/auth/script CRUD commands

Rejected.

Those commands would make frequent edits more mechanical and push Courier away
from its text-first editing model.

## Known Limitations

- Vars and Scripts are still TOML-backed sections; the editor only improves
  grouping and projection.
- Tests are plain text in the section view but still serialize back into TOML
  front matter.
- Request editing still shows one active section at a time rather than a full
  multi-pane inspector.
