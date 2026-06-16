# 005 Request Meta Name Description v1

## Background

`courier-new-request` already asks for the URL before opening a draft, but the
URL should remain the request target rather than becoming hidden metadata. A
draft still needs a temporary buffer identity before it is saved, and users need
a clear place to provide a short request name or a localized note before first
save.

Some request names also need a short filesystem-friendly identity while still
allowing a human note in another language.

## Decision

New request drafts keep an `Untitled N` temporary buffer identity and do not set
request `name` from the URL. Users can set `name` before saving in a `Meta`
section, alongside an optional `description` field.

First save uses the current `name` as the filename prompt default. If the name
is empty or missing, Courier falls back to `request`; it does not read the URL
to infer a filename default.

`courier-open` keeps filesystem paths as the primary candidate text and shows
the front matter `description` as an annotation when present.

## Rationale

- The URL is already visible in the request header and should not silently
  create request metadata.
- Keeping filename defaults tied to `name` gives users one explicit editable
  source of identity.
- `description` separates a human-readable note from the short request name and
  filesystem path.
- The request picker remains path-first for Git-friendly navigation while still
  surfacing notes.

## Alternatives Considered

### Infer name or filename from URL

Rejected.

That creates surprising metadata from request target text. Once users have a
`Meta` section, they can set the name explicitly before saving.

### Prompt for description during request creation

Rejected for now.

The creation flow should stay lightweight. The `Meta` section gives users a
place to add a note before saving without adding another mandatory prompt.

### Use description for filename defaults

Rejected.

Descriptions may be localized or sentence-like. Filenames should come from the
short request name.

## Known Limitations

- Users who leave `name` empty get `request` as the first-save filename
  default and can still edit the filename prompt.
- `courier-open` reads front matter for annotations, but it still does not parse
  the HTTP request block to build candidates.
