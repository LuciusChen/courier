# 003 Response Error Boundary v1

## Background

Courier's response workflow grew around the happy path:

- send one curl request
- parse headers, body, and write-out metadata
- apply post-response vars and scripts
- run response tests
- render one response buffer

That shape worked while failures were mostly treated as curl transport errors
or explicit non-2xx HTTP responses. Real use exposed a messier boundary.

In the incident that triggered this postmortem, the request URL used
`https://` for a port that only served `http://`. The service never received a
usable request, but Courier made that failure hard to diagnose:

1. transport errors could render as an empty response body
2. TLS protocol mismatches did not point clearly to an `https://` vs `http://`
   mistake
3. request and response temp files could be confused on failure paths
4. local response-processing failures could escape from the process sentinel
   instead of becoming a visible response state
5. post-response side effects could persist runtime vars even when the
   response was unsuccessful

This is a response-workflow problem, not just an error-message problem.
Courier needs a clearer rule for which failures become response states, which
side effects are allowed on failure, and where local processing errors should
surface.

## Decision

Keep the existing response-buffer model, but make the response boundary
explicit:

- once curl has started, Courier should always converge to one visible response
  state in the response buffer
- transport failures should surface as request errors with user-facing context,
  not as empty bodies
- local failures that happen after curl completes but before rendering finishes
  should become explicit response errors, not uncaught sentinel failures
- request payload staging and response body storage must use distinct temp
  files
- post-response runtime side effects should run only for successful 2xx
  responses

For v1, "successful" means:

- curl exit code is zero
- HTTP status is 2xx

Failures before curl starts during the send workflow should also converge to a
visible response state. That includes request-view sync errors, request
validation failures, and pre-request script failures.

## Rationale

- The user cares first about whether Courier actually sent a request and what
  happened next. An empty response buffer is worse than an explicit error
  because it misleads diagnosis.
- The response buffer is already Courier's primary execution surface after a
  send. Once a request crosses into the async curl path, all downstream
  failures should stay in that same UI.
- Transport failure, HTTP failure, and local post-response failure are
  different classes of problems, but they all need one visible terminal state.
- Runtime vars are part of Courier's persistent state. Writing them from a
  failed response quietly pollutes later requests and makes failures leak into
  success paths.
- Request and response payloads have different semantics. Sharing temp files
  between them makes debugging and cleanup logic unreliable.

## Alternatives Considered

### Keep response rendering best-effort and leave local failures in the echo area

Rejected.

This preserves the current ambiguity. It makes response processing look like a
UI-only concern even though it controls tests, runtime vars, and the user's
main debugging surface.

### Keep pre-send send-workflow failures as command errors

Rejected.

This keeps the same ambiguity as the original incident. If the user pressed
send and Courier stopped before curl started, the execution still failed
within the request workflow and should surface in the same response UI instead
of splitting diagnosis between a response buffer and ephemeral command errors.

### Allow post-response vars on any HTTP response, including non-2xx

Rejected.

That may be useful for some APIs, but it is too error-prone as the default.
Persisting state from failed responses is the more dangerous behavior in v1.
If Courier later wants opt-in extraction from non-2xx responses, that should
be a deliberate feature with explicit semantics.

## Known Limitations

- Response-processing errors can preserve the underlying HTTP status for
  context, so callers must treat the response reason and status face as the
  source of truth for local failure classification.
