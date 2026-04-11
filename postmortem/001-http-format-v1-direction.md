# 001 `.http` Format v1 Direction

## Background

Courier currently stores request metadata inline with the raw HTTP request
using ad hoc directives such as:

- `# @name`
- `# @auth`
- `# @var`
- `# @test`
- `# @begin pre-request`
- `# @begin post-response`

That syntax was workable while Courier behaved mostly like a raw text editor
with a few send/preview commands. It stops fitting once the request-side UI
wants to behave like the response-side UI, where switching sections replaces
the visible content instead of jumping to a line in the source file.

The current directive model creates three problems:

1. Request views do not map cleanly to source blocks.
2. Missing sections tend to surface as "no directive found" navigation errors
   instead of empty editable states.
3. The parser and editor grow more brittle each time more metadata is encoded
   as scattered directives.

## Decision

Courier should keep `.http` as the file extension, but move toward a new
request-file structure:

- TOML front matter for Courier metadata
- one raw HTTP request block for method, URL, headers, and body

Query params remain part of the URL. Courier may offer a structured params
editor, but saving must still write back into the URL rather than creating a
second stored params section.

The normative format definition lives in [HTTP-FORMAT-V1.md](../HTTP-FORMAT-V1.md).

## Rationale

- `.http` remains readable and Git-friendly.
- Raw HTTP editing stays natural for method, URL, headers, and body.
- Courier-specific metadata becomes structured, which makes parsing,
  validation, and round-tripping more robust.
- Request-side views can become source-backed views instead of line-jump
  helpers.
- Missing sections can become empty states instead of parse/navigation errors.
- TOML is stricter and easier to round-trip safely than YAML for this use case.

## Alternatives Considered

### Keep expanding the directive syntax

Rejected.

This preserves short-term compatibility but keeps mixing two different kinds of
data in one flat preamble. Every new request-side feature increases parser
complexity and weakens the mapping between UI and file structure.

### Move the whole request into YAML

Rejected.

That matches Bruno more directly, but it makes raw HTTP editing worse,
especially for headers and larger JSON or XML bodies. Courier should stay
plain-text and Emacs-native rather than become a YAML form editor.

### Use a sidecar metadata file per request

Rejected.

That breaks the single-file mental model, complicates Git review, and makes it
easier for request content and request metadata to drift apart.

## Known Limitations

- The current implementation still uses directive syntax. This postmortem sets
  direction; it does not mean the migration already exists.
- Courier will need a dual-parser migration period if existing collections are
  to keep working without manual rewrites.
- The exact migration UX still needs a dedicated plan: one-shot rewrite,
  on-save conversion, or explicit command.
