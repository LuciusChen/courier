# 004 New Request Prompt Flow v1

## Background

`courier-new-request` originally opened an unsaved draft immediately with an
empty request line such as `GET `. That matched Courier's text-first editing
model, but it made the first screen ambiguous: the user had to notice that point
was sitting after the method token and then type the URL manually.

In practice, the URL is the first meaningful piece of identity for a request.
The method matters, but it is secondary enough that Courier can default it and
let the user confirm or change it during creation.

## Decision

`courier-new-request` now prompts in this order:

- request URL
- HTTP method, defaulting to `courier-default-request-method`

Courier then opens the same unsaved draft buffer as before, with the request
line already populated.

## Rationale

- Asking for the URL first follows the user's likely mental model: choose the
  endpoint, then choose how to call it.
- Keeping method selection in the creation flow avoids making `POST` users
  create a `GET` draft and immediately run a second command.
- The on-disk `.http` format stays unchanged. This is only an entry workflow
  change.
- Save behavior stays unchanged: first save still asks which collection should
  own the draft, then asks for the request filename.

## Alternatives Considered

### Keep opening a blank draft

Rejected.

The old flow was flexible, but the first action after creating a request was
always to fill the URL. Making that explicit reduces an avoidable pause.

### Prompt for method first

Rejected.

HTTP clients commonly foreground the URL because it identifies the target. The
method is important, but `GET` remains a useful default and a later prompt is
enough.

### Infer the method from typed input

Rejected for now.

Accepting strings such as `POST https://example.com` could be useful, but it
also creates a second mini-parser before the real request parser. A separate
method prompt keeps the flow explicit.

## Known Limitations

- Creating a request now requires a non-empty URL without whitespace. Users who
  want a scratch request can still edit any existing request buffer or create a
  draft with a placeholder URL.
- The generated draft name remains `Untitled N`; Courier does not infer a name
  from the URL.
