#!/usr/bin/env python3
"""Minimal local HTTP server for Courier integration tests."""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import signal
import sys
from email.parser import BytesParser
from email.policy import HTTP
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qsl, urlsplit


PNG_BYTES = base64.b64decode(
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO9Z7WQAAAAASUVORK5CYII="
)

PDF_BYTES = b"""%PDF-1.1
1 0 obj
<< /Type /Catalog /Pages 2 0 R >>
endobj
2 0 obj
<< /Type /Pages /Count 1 /Kids [3 0 R] >>
endobj
3 0 obj
<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 200] /Contents 4 0 R >>
endobj
4 0 obj
<< /Length 35 >>
stream
BT /F1 18 Tf 40 120 Td (Courier) Tj ET
endstream
endobj
xref
0 5
0000000000 65535 f 
0000000009 00000 n 
0000000058 00000 n 
0000000115 00000 n 
0000000202 00000 n 
trailer
<< /Root 1 0 R /Size 5 >>
startxref
286
%%EOF
"""

SVG_BYTES = b"""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
<rect width="10" height="10" fill="#5b8c5a"/>
</svg>
"""

HTML_BYTES = b"""<!doctype html><html><body><h1>Courier</h1></body></html>"""


def json_bytes(payload: object) -> bytes:
    return json.dumps(payload, sort_keys=True).encode("utf-8")


def parse_multipart(body: bytes, content_type: str) -> list[dict[str, object]]:
    message = BytesParser(policy=HTTP).parsebytes(
        ("Content-Type: %s\r\nMIME-Version: 1.0\r\n\r\n" % content_type).encode("latin-1")
        + body
    )
    parts: list[dict[str, object]] = []
    for part in message.iter_parts():
        content_disposition = part.get("Content-Disposition", "")
        name = part.get_param("name", header="Content-Disposition")
        filename = part.get_param("filename", header="Content-Disposition")
        content = part.get_payload(decode=True) or b""
        entry: dict[str, object] = {
            "name": name,
            "filename": filename,
            "content_type": part.get_content_type(),
            "size": len(content),
            "sha256": hashlib.sha256(content).hexdigest(),
            "kind": "file" if filename else "text",
        }
        if not filename:
            entry["value"] = content.decode("utf-8")
        parts.append(entry)
    return parts


class CourierTestHandler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, fmt: str, *args: object) -> None:
        sys.stderr.write("%s - - [%s] %s\n" % (self.client_address[0], self.log_date_time_string(), fmt % args))

    def _read_body(self) -> bytes:
        length = int(self.headers.get("Content-Length", "0"))
        return self.rfile.read(length) if length else b""

    def _send_bytes(
        self,
        status: int,
        body: bytes,
        *,
        content_type: str,
        extra_headers: dict[str, str] | None = None,
    ) -> None:
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        if extra_headers:
            for key, value in extra_headers.items():
                self.send_header(key, value)
        self.end_headers()
        self.wfile.write(body)

    def _send_json(self, status: int, payload: object) -> None:
        self._send_bytes(status, json_bytes(payload), content_type="application/json")

    def _request_summary(self, body: bytes | None = None) -> dict[str, object]:
        split = urlsplit(self.path)
        return {
            "method": self.command,
            "path": split.path,
            "query": dict(parse_qsl(split.query, keep_blank_values=True)),
            "headers": {key.lower(): value for key, value in self.headers.items()},
            "body_text": (body or b"").decode("utf-8", errors="replace"),
            "body_size": len(body or b""),
            "body_sha256": hashlib.sha256(body or b"").hexdigest(),
        }

    def do_GET(self) -> None:
        if self.path.startswith("/echo"):
            self._send_json(200, self._request_summary())
            return
        if self.path.startswith("/redirect"):
            self.send_response(302)
            self.send_header("Location", "/echo?redirected=1")
            self.send_header("Content-Length", "0")
            self.end_headers()
            return
        if self.path.startswith("/protected"):
            auth = self.headers.get("Authorization")
            if auth == "Bearer courier-test-token":
                self._send_json(200, {"ok": True, "authorization": auth})
            else:
                self._send_json(401, {"ok": False, "authorization": auth})
            return
        if self.path.startswith("/html"):
            self._send_bytes(200, HTML_BYTES, content_type="text/html; charset=utf-8")
            return
        if self.path.startswith("/image/png"):
            self._send_bytes(200, PNG_BYTES, content_type="image/png")
            return
        if self.path.startswith("/image/svg"):
            self._send_bytes(200, SVG_BYTES, content_type="image/svg+xml")
            return
        if self.path.startswith("/pdf"):
            self._send_bytes(200, PDF_BYTES, content_type="application/pdf")
            return
        if self.path.startswith("/bad-charset"):
            self._send_bytes(
                200,
                b"hello",
                content_type="text/plain; charset=x-courier-invalid",
            )
            return
        self._send_json(404, {"error": "not_found", "path": self.path})

    def do_POST(self) -> None:
        body = self._read_body()
        if self.path.startswith("/echo"):
            self._send_json(200, self._request_summary(body))
            return
        if self.path.startswith("/binary"):
            self._send_json(
                200,
                {
                    **self._request_summary(body),
                    "content_type": self.headers.get("Content-Type"),
                },
            )
            return
        if self.path.startswith("/multipart"):
            self._send_json(
                200,
                {
                    **self._request_summary(body),
                    "parts": parse_multipart(body, self.headers.get("Content-Type", "")),
                },
            )
            return
        if self.path.startswith("/oauth/token"):
            form = dict(parse_qsl(body.decode("utf-8"), keep_blank_values=True))
            if (
                form.get("grant_type") == "client_credentials"
                and form.get("client_id") == "courier-client"
                and form.get("client_secret") == "courier-secret"
            ):
                self._send_json(
                    200,
                    {
                        "access_token": "courier-test-token",
                        "token_type": "Bearer",
                        "scope": form.get("scope", ""),
                    },
                )
            else:
                self._send_json(401, {"error": "invalid_client", "form": form})
            return
        self._send_json(404, {"error": "not_found", "path": self.path})


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=0)
    parser.add_argument("--port-file", required=True)
    args = parser.parse_args()

    server = ThreadingHTTPServer((args.host, args.port), CourierTestHandler)
    Path(args.port_file).write_text(str(server.server_port), encoding="utf-8")

    def shutdown_handler(_signum: int, _frame: object) -> None:
        server.shutdown()

    signal.signal(signal.SIGTERM, shutdown_handler)
    signal.signal(signal.SIGINT, shutdown_handler)
    server.serve_forever()
    server.server_close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
