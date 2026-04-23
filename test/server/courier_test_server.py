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

CYBERPUNK_SVG_BYTES = b"""<svg xmlns="http://www.w3.org/2000/svg" width="1280" height="720" viewBox="0 0 1280 720" fill="none">
<defs>
  <linearGradient id="bg" x1="80" y1="40" x2="1120" y2="680" gradientUnits="userSpaceOnUse">
    <stop stop-color="#08111f"/>
    <stop offset="0.55" stop-color="#111827"/>
    <stop offset="1" stop-color="#190b2d"/>
  </linearGradient>
  <linearGradient id="skyGlow" x1="240" y1="120" x2="1000" y2="540" gradientUnits="userSpaceOnUse">
    <stop stop-color="#33d2ff" stop-opacity="0.55"/>
    <stop offset="1" stop-color="#ff3ca6" stop-opacity="0.2"/>
  </linearGradient>
  <linearGradient id="visor" x1="0" y1="0" x2="1" y2="1">
    <stop stop-color="#93f8ff"/>
    <stop offset="1" stop-color="#36a4ff"/>
  </linearGradient>
  <linearGradient id="coat" x1="520" y1="300" x2="810" y2="640" gradientUnits="userSpaceOnUse">
    <stop stop-color="#1d3557"/>
    <stop offset="1" stop-color="#0f172a"/>
  </linearGradient>
  <filter id="softGlow" x="-20%" y="-20%" width="140%" height="140%">
    <feGaussianBlur stdDeviation="8"/>
  </filter>
</defs>

<rect width="1280" height="720" fill="url(#bg)"/>
<rect x="0" y="0" width="1280" height="720" fill="url(#skyGlow)" opacity="0.75"/>

<g opacity="0.22">
  <path d="M0 530H1280" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M0 570H1280" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M0 610H1280" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M0 650H1280" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M0 690H1280" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M160 500V720" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M300 500V720" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M440 500V720" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M580 500V720" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M720 500V720" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M860 500V720" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M1000 500V720" stroke="#7dd3fc" stroke-width="1"/>
  <path d="M1140 500V720" stroke="#7dd3fc" stroke-width="1"/>
</g>

<g opacity="0.7">
  <rect x="70" y="240" width="84" height="300" fill="#09131f"/>
  <rect x="168" y="190" width="110" height="350" fill="#0b1725"/>
  <rect x="290" y="265" width="92" height="275" fill="#0c1b2d"/>
  <rect x="396" y="170" width="66" height="370" fill="#0a1320"/>
  <rect x="930" y="210" width="70" height="330" fill="#0a1422"/>
  <rect x="1016" y="158" width="120" height="382" fill="#0b1725"/>
  <rect x="1148" y="248" width="76" height="292" fill="#0a1320"/>
</g>

<g opacity="0.95">
  <rect x="188" y="214" width="10" height="220" fill="#61dafb"/>
  <rect x="221" y="214" width="10" height="260" fill="#ff4db8"/>
  <rect x="1054" y="186" width="12" height="248" fill="#61dafb"/>
  <rect x="1091" y="186" width="12" height="214" fill="#ffd166"/>
  <rect x="1155" y="268" width="10" height="138" fill="#ff4db8"/>
</g>

<circle cx="906" cy="154" r="110" fill="#2dd4bf" opacity="0.08" filter="url(#softGlow)"/>
<circle cx="906" cy="154" r="58" stroke="#7dd3fc" stroke-width="2" opacity="0.6"/>
<circle cx="906" cy="154" r="18" fill="#7dd3fc" opacity="0.9"/>

<g transform="translate(448 122)">
  <path d="M180 124C214 124 241 96 241 60C241 24 214 0 180 0C146 0 119 24 119 60C119 96 146 124 180 124Z" fill="#d9e7ff"/>
  <path d="M118 70C134 22 170 -4 208 4C182 -4 156 10 143 38C132 60 130 84 135 109C121 100 113 85 118 70Z" fill="#f7b0d5" opacity="0.45"/>
  <path d="M128 62H234C225 88 204 103 180 103C156 103 136 88 128 62Z" fill="url(#visor)"/>
  <path d="M122 62C137 53 160 48 180 48C200 48 223 53 238 62" stroke="#dbeafe" stroke-width="5" stroke-linecap="round"/>
  <path d="M150 120C129 135 110 158 98 188L162 204C168 178 175 152 180 124L150 120Z" fill="#d8e7ff"/>
  <path d="M210 120C231 135 250 158 262 188L198 204C192 178 185 152 180 124L210 120Z" fill="#d8e7ff"/>
  <path d="M106 188C126 140 170 128 180 128C190 128 234 140 254 188L284 424H76L106 188Z" fill="url(#coat)"/>
  <path d="M178 132L148 232L180 282L212 232L182 132H178Z" fill="#172554"/>
  <path d="M135 218L164 206L180 282L116 424H76L135 218Z" fill="#0f172a"/>
  <path d="M225 218L196 206L180 282L244 424H284L225 218Z" fill="#111827"/>
  <path d="M180 149L194 194L180 218L166 194L180 149Z" fill="#7dd3fc"/>
  <path d="M148 319H221" stroke="#61dafb" stroke-width="3" stroke-linecap="round" opacity="0.85"/>
  <path d="M139 344H229" stroke="#ff4db8" stroke-width="2" stroke-linecap="round" opacity="0.75"/>
  <path d="M130 369H238" stroke="#61dafb" stroke-width="2" stroke-linecap="round" opacity="0.7"/>
  <circle cx="151" cy="319" r="5" fill="#7dd3fc"/>
  <circle cx="212" cy="344" r="4" fill="#ff4db8"/>
  <circle cx="171" cy="369" r="4" fill="#7dd3fc"/>
  <path d="M120 225C136 218 149 220 161 229" stroke="#7dd3fc" stroke-width="2"/>
  <path d="M239 225C223 218 210 220 198 229" stroke="#7dd3fc" stroke-width="2"/>
</g>

<g opacity="0.86">
  <path d="M635 188L774 188" stroke="#61dafb" stroke-width="2"/>
  <path d="M635 215L896 215" stroke="#ff4db8" stroke-width="2"/>
  <path d="M635 242L866 242" stroke="#61dafb" stroke-width="2"/>
  <path d="M635 269L818 269" stroke="#ffd166" stroke-width="2"/>
  <path d="M635 296L891 296" stroke="#61dafb" stroke-width="2"/>
</g>

<g opacity="0.75" font-family="Menlo, Monaco, monospace">
  <text x="80" y="86" fill="#e5f4ff" font-size="42" font-weight="700">Courier Demo // Neon Shell</text>
  <text x="84" y="120" fill="#8fdcff" font-size="18">Original cyberpunk SVG for image-response screenshots</text>
  <text x="640" y="188" fill="#e2e8f0" font-size="18">TACTICAL FEED</text>
  <text x="640" y="211" fill="#8fdcff" font-size="14">subject: synthetic field operative</text>
  <text x="640" y="238" fill="#f9a8d4" font-size="14">status: active / urban network sync online</text>
  <text x="640" y="265" fill="#8fdcff" font-size="14">threat map: shinjuku-sector analog // simulated</text>
  <text x="640" y="292" fill="#fde68a" font-size="14">note: designed for Courier image and timeline demos</text>
</g>
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
        send_body: bool = True,
    ) -> None:
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        if extra_headers:
            for key, value in extra_headers.items():
                self.send_header(key, value)
        self.end_headers()
        if send_body:
            self.wfile.write(body)

    def _send_json(
        self,
        status: int,
        payload: object,
        *,
        extra_headers: dict[str, str] | None = None,
        send_body: bool = True,
    ) -> None:
        self._send_bytes(
            status,
            json_bytes(payload),
            content_type="application/json",
            extra_headers=extra_headers,
            send_body=send_body,
        )

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

    def _send_request_summary(
        self,
        body: bytes | None = None,
        *,
        send_body: bool = True,
        extra_headers: dict[str, str] | None = None,
    ) -> None:
        self._send_json(
            200,
            self._request_summary(body),
            extra_headers=extra_headers,
            send_body=send_body,
        )

    def _send_status_response(self, *, send_body: bool = True) -> bool:
        split = urlsplit(self.path)
        parts = split.path.strip("/").split("/")
        if len(parts) != 2 or parts[0] != "status":
            return False
        try:
            status = int(parts[1])
        except ValueError:
            status = 400
        self._send_json(status, {"status": status, "path": split.path}, send_body=send_body)
        return True

    def do_GET(self) -> None:
        if self._send_status_response():
            return
        if self.path.startswith("/echo"):
            self._send_request_summary()
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
        if self.path.startswith("/image/cyberpunk"):
            self._send_bytes(200, CYBERPUNK_SVG_BYTES, content_type="image/svg+xml")
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
        if self._send_status_response():
            return
        if self.path.startswith("/echo"):
            self._send_request_summary(body)
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

    def _handle_summary_method(self) -> None:
        body = self._read_body()
        if self._send_status_response():
            return
        if self.path.startswith("/echo"):
            self._send_request_summary(body)
            return
        self._send_json(404, {"error": "not_found", "path": self.path})

    def do_PUT(self) -> None:
        self._handle_summary_method()

    def do_PATCH(self) -> None:
        self._handle_summary_method()

    def do_DELETE(self) -> None:
        self._handle_summary_method()

    def do_OPTIONS(self) -> None:
        self._handle_summary_method()

    def do_HEAD(self) -> None:
        if self._send_status_response(send_body=False):
            return
        if self.path.startswith("/echo"):
            self._send_request_summary(
                send_body=False,
                extra_headers={"X-Courier-Method": "HEAD"},
            )
            return
        self._send_json(
            404,
            {"error": "not_found", "path": self.path},
            send_body=False,
        )


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
