# Courier `.http` Format v1

This document defines the current Courier request-file format.

## Goals

- Keep request files as plain text that users can read and edit directly.
- Keep one source of truth per request file.
- Separate Courier metadata from raw HTTP content.
- Make request-side views map cleanly back to the file format.
- Make parsing and round-tripping deterministic.

## Summary

A Courier v1 request file stays a `.http` file and has two parts:

1. Optional TOML front matter for Courier metadata
2. One raw HTTP request block

The HTTP request block remains the source of truth for:

- method
- URL
- headers
- body

The TOML front matter is the source of truth for:

- name
- timeout
- follow redirects
- body type
- auth
- vars
- tests
- pre-request script
- post-response script

Query params do not become a second source of truth. They stay part of the URL.
Courier may offer a structured editor for query params, but saving must write
back into the URL.

## File Shape

```text
+++
# TOML metadata
+++
METHOD URL
Header: Value

Body
```

The front matter is optional. If omitted, the file is still valid as long as
the raw HTTP request block is valid.

## Example

```text
+++
name = "Create User"
timeout = 10
follow_redirects = true

[body]
type = "json"

[auth]
type = "bearer"
token = "{{token}}"

[vars]
token = "abc123"
tenant = "acme"

tests = [
  "status == 201",
  "header content-type contains json",
]

[scripts]
pre_request = """
setq courier-script-request
  (plist-put courier-script-request :url
             (concat (plist-get courier-script-request :url) "?preview=1"))
courier-script-request
"""

post_response = """
setq courier-script-response
  (plist-put courier-script-response :reason "Handled")
courier-script-response
"""
+++
POST https://api.example.com/users?tenant={{tenant}}
Accept: application/json
Content-Type: application/json

{
  "name": "Lucy"
}
```

## Front Matter Rules

### Delimiters

- Front matter begins with a line containing exactly `+++`
- Front matter ends with the next line containing exactly `+++`
- Only one front matter block is allowed

### Encoding

- Files are UTF-8 text
- TOML front matter is parsed as UTF-8
- The HTTP block is stored as plain text and is not re-encoded by Courier

### Supported Keys

Top-level keys:

- `name`
- `timeout`
- `follow_redirects`
- `tests`

Tables:

- `[body]`
- `[auth]`
- `[vars]`
- `[scripts]`

Unknown keys should be preserved when possible and surfaced as unsupported in
the UI instead of being silently dropped.

### `name`

```toml
name = "Create User"
```

- Optional string
- Used for display and file creation defaults

### `timeout`

```toml
timeout = 10
```

- Optional integer in seconds
- Must be greater than zero

### `follow_redirects`

```toml
follow_redirects = true
```

- Optional boolean

### `[body]`

JSON body:

```toml
[body]
type = "json"
```

Allowed values in v1:

- `none`
- `json`
- `xml`
- `text`
- `form-urlencoded`
- `multipart`
- `binary`

Rules:

- `type` is required when `[body]` exists
- invalid body tables are parse errors
- `type = "none"` requires an empty HTTP body block
- `form-urlencoded` still stores its source of truth in the HTTP body block;
  Courier may offer a structured editor, but saving must write back to the same
  body block
- `multipart` stores its source of truth in repeated `[[body.parts]]` tables
- `binary` stores its source of truth in `[body].path` and optional
  `[body].content_type`
- Courier may add a default `Content-Type` at send time when the request does
  not declare one explicitly:
  - `json` -> `application/json`
  - `xml` -> `application/xml`
  - `text` -> `text/plain; charset=utf-8`
  - `form-urlencoded` -> `application/x-www-form-urlencoded`

Multipart body:

```toml
[body]
type = "multipart"

[[body.parts]]
name = "avatar"
kind = "file"
path = "./avatar.png"
content_type = "image/png"

[[body.parts]]
name = "display_name"
kind = "text"
value = "Lucy"
```

Rules:

- every part requires `name`
- `kind` must be `text` or `file`
- `text` parts require `value`
- `file` parts require `path`
- `content_type` is optional

Binary body:

```toml
[body]
type = "binary"
path = "./payload.bin"
content_type = "application/octet-stream"
```

Rules:

- `path` is required
- `content_type` is optional

### `[auth]`

Bearer auth:

```toml
[auth]
type = "bearer"
token = "{{token}}"
```

Basic auth:

```toml
[auth]
type = "basic"
username = "{{user}}"
password = "{{password}}"
```

Header auth:

```toml
[auth]
type = "header"
header = "X-API-Key"
value = "{{token}}"
```

API key auth:

```toml
[auth]
type = "api_key"
in = "header"
name = "x-api-key"
value = "{{api_key}}"
```

OAuth2 auth:

```toml
[auth]
type = "oauth2"
grant_type = "client_credentials"
token_url = "https://example.com/oauth/token"
client_id = "{{client_id}}"
client_secret = "{{client_secret}}"
scopes = ["read", "write"]
```

Rules:

- `type` is required when `[auth]` exists
- allowed values: `none`, `bearer`, `basic`, `header`, `api_key`, `oauth2`
- invalid auth tables are parse errors
- `api_key` requires:
  - `in = "header"` or `in = "query"`
  - `name`
  - `value`
- `oauth2` currently supports only:
  - `grant_type = "client_credentials"`
  - `token_url`
  - `client_id`
  - `client_secret`
  - optional `scopes`
- `oauth2 client_credentials` has real send-time semantics:
  - Courier first sends a token request to `token_url`
  - the token request uses `POST` and `application/x-www-form-urlencoded`
  - `grant_type`, `client_id`, `client_secret`, and optional `scope` are
    encoded in the token request body
  - Courier extracts `access_token` from the JSON response body
  - Courier injects `Authorization: Bearer <access_token>` into the main
    request unless that header was already set explicitly

### `[vars]`

```toml
[vars]
token = "abc123"
tenant = "acme"
```

Rules:

- string values only in v1
- request vars override env vars with the same name

### `tests`

```toml
tests = [
  "status == 200",
  "body contains hello",
]
```

Rules:

- optional array of strings
- each string is one Courier test expression
- test syntax stays the same as the current runtime

### `[scripts]`

```toml
[scripts]
pre_request = """
...
"""
post_response = """
...
"""
```

Rules:

- both keys are optional
- values are multiline strings
- script language remains Emacs Lisp

## Raw HTTP Block Rules

The request block begins immediately after the closing `+++`, or at the start
of the file if no front matter exists.

It has the usual shape:

```text
METHOD URL
Header: Value

Body
```

Rules:

- exactly one request per file
- first non-empty line must be `METHOD URL`
- allowed methods remain the current Courier method set
- header lines continue until the first empty line
- everything after the first empty line is body text

## Query Params

Query params remain part of the URL:

```text
GET https://api.example.com/users?page=1&sort=created_at
```

Courier may provide a structured query-param editor, but:

- it must parse from the URL
- it must write back into the URL
- it must not create a second stored params section

## View Mapping

This format is designed so request-side views map directly to one source block:

- `URL` view -> request line
- `Headers` view -> header block
- `Body` view -> body block
- `Body` type selector -> `[body].type`
- `Params` view -> parsed URL query, saved back into URL
- `Auth` view -> `[auth]`
- `Vars` view -> `[vars]`
- `Script` view -> `[scripts]`
- `Tests` view -> `tests`

Missing sections are empty states, not errors.

## Round-Trip Rules

Courier must be able to parse and save the file without inventing hidden state.

### Saving

- preserve the `.http` file as the only source of truth
- emit front matter keys in a stable order
- emit exactly one blank line between closing `+++` and request line
- preserve body text exactly
- normalize line endings to the file's existing convention if possible

### Stable Key Order

When Courier writes front matter, use this order:

1. `name`
2. `timeout`
3. `follow_redirects`
4. `tests`
5. `[body]`
6. `[auth]`
7. `[vars]`
8. `[scripts]`

### Missing Values

- omit empty sections instead of emitting empty tables
- do not emit `[auth]` if no auth is configured
- do not emit `tests = []` unless the user explicitly created an empty list and
  Courier needs to preserve that state

## Validation Rules

Parse errors should be explicit and include location whenever possible.

Examples:

- unclosed front matter block
- invalid TOML
- invalid auth table
- invalid request line
- duplicate front matter sections that conflict

Missing optional sections are not parse errors.

## Migration from Current Syntax

Current Courier syntax uses directives such as:

- `# @name`
- `# @timeout`
- `# @follow-redirects`
- `# @auth`
- `# @var`
- `# @test`
- `# @begin pre-request`
- `# @begin post-response`

Courier v1 should provide a migration command that rewrites them into front
matter.

Example migration:

```text
# @name Create User
# @auth bearer {{token}}
# @var token abc123
# @test status == 201
# @begin pre-request
# ...
# @end
POST https://api.example.com/users
Accept: application/json
```

becomes:

```text
+++
name = "Create User"

[auth]
type = "bearer"
token = "{{token}}"

[vars]
token = "abc123"

tests = ["status == 201"]

[scripts]
pre_request = """
...
"""
+++
POST https://api.example.com/users
Accept: application/json
```

## Non-Goals

- No sidecar metadata file per request
- No separate params storage
- No database-backed request state
- No YAML request body wrapping
- No duplication between UI state and file state

## Decision

Courier v1 should keep `.http` as the file extension and move to:

- TOML front matter for Courier metadata
- raw HTTP text for the actual request

This is the best fit for:

- readability
- maintainability
- deterministic parsing
- robust round-tripping
- an Emacs-native request UI
