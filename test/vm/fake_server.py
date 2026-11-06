#!/usr/bin/env python3
"""Fake MCP server for the network test.

Serves the subset of the stateless 2026-07-28 revision that mcp.el uses:
server/discover, paginated tools/list with ttlMs, tools/call including a
multi round-trip input request and a server-sent event response, plus the
prompts and resources lists.  It records what it observed -- required
headers, per-request metadata, duplicate JSON keys -- and reports that on
GET /stats, so the client can assert on the server side of the wire.

Run as a Streamable HTTP server or as a stdio server:

    fake_server.py --transport http --port 8001 --name a
    fake_server.py --transport stdio --name stdio-srv
"""

import argparse
import base64
import json
import sys
import threading
import time
from collections import Counter
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

PROTOCOL_VERSION = "2026-07-28"
HEADER_PREFIX = "=?base64?"
HEADER_SUFFIX = "?="
LIST_TTL_MS = 60000


def object_schema(properties, required=()):
    schema = {"type": "object", "properties": properties}
    if required:
        schema["required"] = list(required)
    return schema


def tool(name, properties, required=()):
    return {
        "name": name,
        "description": f"{name} from the network test",
        "inputSchema": object_schema(properties, required),
    }


PAGE_ONE = [
    tool("echo", {"text": {"type": "string"}}, ("text",)),
    tool(
        "sql",
        {
            "region": {"type": "string", "x-mcp-header": "Region"},
            "query": {"type": "string"},
        },
        ("region", "query"),
    ),
    tool("flag", {"on": {"type": "boolean", "x-mcp-header": "On"}}),
    tool("roots", {}),
    tool("stream", {}),
    tool("slow", {}),
]

PAGE_TWO = [
    tool("utf8-世界", {}),
    tool('quote"name', {}),
    tool("after-two", {}),
]


def complete_text(text):
    return {"resultType": "complete", "content": [{"type": "text", "text": text}]}


def decode_header(value):
    if value.startswith(HEADER_PREFIX) and value.endswith(HEADER_SUFFIX):
        encoded = value[len(HEADER_PREFIX) : -len(HEADER_SUFFIX)]
        return base64.b64decode(encoded).decode("utf-8")
    return value


class State:
    """What the fake server observed on the wire."""

    def __init__(self, name):
        self.name = name
        self.counts = Counter()
        self.violations = []
        self.sse_sent = 0
        self.lock = threading.Lock()

    def count(self, method):
        with self.lock:
            self.counts[method] += 1

    def violation(self, kind, detail):
        with self.lock:
            self.violations.append(f"{kind}: {detail}")


def parse_body(raw, state):
    """Parse RAW and record every object that repeats one of its keys."""
    duplicates = []

    def hook(pairs):
        keys = [key for key, _ in pairs]
        for key in keys:
            if keys.count(key) > 1:
                duplicates.append(key)
        return dict(pairs)

    message = json.loads(raw, object_pairs_hook=hook)
    for key in duplicates:
        state.violation("duplicate-key", key)
    return message


def check_request(state, method, params, headers):
    """Record the protocol violations of one request."""
    meta = params.get("_meta") or {}
    if meta.get("io.modelcontextprotocol/protocolVersion") != PROTOCOL_VERSION:
        state.violation("missing-protocol-version-meta", method)
    if "io.modelcontextprotocol/clientCapabilities" not in meta:
        state.violation("missing-client-capabilities-meta", method)
    if headers is None:
        return
    if headers.get("MCP-Protocol-Version") != PROTOCOL_VERSION:
        state.violation("bad-version-header", headers.get("MCP-Protocol-Version"))
    if headers.get("Mcp-Method") != method:
        state.violation(
            "bad-method-header", f"{headers.get('Mcp-Method')} for {method}"
        )
    if method in ("tools/call", "prompts/get", "resources/read"):
        expected = params.get("name") or params.get("uri")
        seen = decode_header(headers.get("Mcp-Name") or "")
        if seen != expected:
            state.violation("bad-name-header", f"{seen!r} for {expected!r}")
    if method == "tools/call":
        arguments = params.get("arguments") or {}
        mirrors = (
            ("sql", "Mcp-Param-Region", "region"),
            ("flag", "Mcp-Param-On", "on"),
        )
        for tool_name, header, key in mirrors:
            if params.get("name") != tool_name:
                continue
            value = arguments.get(key)
            if isinstance(value, bool):
                expected = "true" if value else "false"
            else:
                expected = None if value is None else str(value)
            seen = decode_header(headers.get(header) or "")
            if seen != (expected or ""):
                state.violation("bad-param-header", f"{header}: {seen!r}")


def call_tool(state, name, params):
    tool_name = params.get("name")
    arguments = params.get("arguments") or {}
    if tool_name == "echo":
        return ("result", complete_text(f"echo[{name}]:{arguments.get('text')}"))
    if tool_name == "sql":
        return (
            "result",
            complete_text(f"sql[{name}]:{arguments.get('region')}:{arguments.get('query')}"),
        )
    if tool_name == "flag":
        shown = "true" if arguments.get("on") else "false"
        return ("result", complete_text(f"flag[{name}]:{shown}"))
    if tool_name == "utf8-世界":
        return ("result", complete_text(f"utf8[{name}]"))
    if tool_name == 'quote"name':
        return ("result", complete_text(f"quote[{name}]"))
    if tool_name == "after-two":
        return ("result", complete_text(f"after-two[{name}]"))
    if tool_name == "roots":
        responses = params.get("inputResponses")
        if not responses:
            return (
                "result",
                {
                    "resultType": "input_required",
                    "inputRequests": {"r1": {"method": "roots/list"}},
                    "requestState": "state-1",
                },
            )
        roots = (responses.get("r1") or {}).get("roots") or []
        uris = ",".join(root.get("uri", "") for root in roots)
        return ("result", complete_text(f"roots[{name}]:{uris}"))
    if tool_name == "stream":
        return ("sse", f"stream[{name}]")
    if tool_name == "slow":
        time.sleep(5)
        return ("result", complete_text(f"slow[{name}]"))
    return ("error", {"code": -32602, "message": f"unknown tool {tool_name}"})


def result_for(state, method, params):
    if method == "server/discover":
        return {
            "supportedVersions": [PROTOCOL_VERSION],
            "capabilities": {"tools": {}, "prompts": {}, "resources": {}},
            "instructions": "fake server for the network test",
            "_meta": {
                "io.modelcontextprotocol/serverInfo": {
                    "name": state.name,
                    "version": "0.1.0",
                }
            },
        }
    if method == "tools/list":
        if params.get("cursor") == "page-2":
            return {"tools": PAGE_TWO, "ttlMs": LIST_TTL_MS}
        return {
            "tools": PAGE_ONE,
            "nextCursor": "page-2",
            "ttlMs": LIST_TTL_MS,
        }
    if method in ("prompts/list", "resources/list", "resources/templates/list"):
        key = {
            "prompts/list": "prompts",
            "resources/list": "resources",
            "resources/templates/list": "resourceTemplates",
        }[method]
        return {key: [], "ttlMs": LIST_TTL_MS}
    raise KeyError(method)


def dispatch(state, message, headers):
    """Return (kind, payload) for a message, or None when it needs no reply."""
    method = message.get("method")
    request_id = message.get("id")
    params = message.get("params") or {}
    if method is None:
        return None
    if request_id is None:
        # A notification, such as notifications/cancelled.
        state.count(method)
        return None
    state.count(method)
    check_request(state, method, params, headers)
    if method == "tools/call":
        return call_tool(state, state.name, params)
    try:
        return ("result", result_for(state, method, params))
    except KeyError:
        return ("error", {"code": -32601, "message": f"no method {method}"})


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"
    server_version = "mcp-fake/0.1"

    def do_GET(self):
        state = self.server.state
        if self.path != "/stats":
            self.respond(404, "text/plain", b"not found")
            return
        body = json.dumps(
            {
                "server": state.name,
                "counts": dict(state.counts),
                "violations": list(state.violations),
                "sse": state.sse_sent,
            }
        ).encode()
        self.respond(200, "application/json", body)

    def do_POST(self):
        state = self.server.state
        length = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(length).decode("utf-8")
        try:
            message = parse_body(raw, state)
        except json.JSONDecodeError:
            self.respond(400, "text/plain", b"bad json")
            return
        outcome = dispatch(state, message, self.headers)
        if outcome is None:
            self.respond(202, "application/json", b"")
            return
        kind, payload = outcome
        if kind == "sse":
            self.stream_sse(state, message, payload)
            return
        if kind == "error":
            body = {"jsonrpc": "2.0", "id": message.get("id"), "error": payload}
        else:
            body = {"jsonrpc": "2.0", "id": message.get("id"), "result": payload}
        self.respond(200, "application/json", json.dumps(body).encode())

    def respond(self, status, content_type, body):
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if body:
            self.wfile.write(body)
        self.wfile.flush()

    def stream_sse(self, state, message, marker):
        request_id = message.get("id")
        state.sse_sent += 1
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Connection", "close")
        self.end_headers()
        self.close_connection = True
        progress = {
            "jsonrpc": "2.0",
            "method": "notifications/progress",
            "params": {"progressToken": "progress-1", "progress": 1, "total": 2},
        }
        reply = {
            "jsonrpc": "2.0",
            "id": request_id,
            "result": {
                "resultType": "complete",
                "structuredContent": {"from": marker},
            },
        }
        try:
            self.wfile.write(b"event: message\ndata: ")
            self.wfile.write(json.dumps(progress).encode() + b"\n\n")
            self.wfile.flush()
            time.sleep(0.2)
            self.wfile.write(b"event: message\ndata: ")
            self.wfile.write(json.dumps(reply).encode() + b"\n\n")
            self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError):
            pass

    def log_message(self, format_string, *args):
        # Keep the systemd journal readable.
        pass


def serve_stdio(state):
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            message = parse_body(line, state)
        except json.JSONDecodeError:
            continue
        outcome = dispatch(state, message, None)
        if outcome is None:
            continue
        kind, payload = outcome
        if kind == "sse":
            # No stream on stdin and stdout: answer with the same content.
            payload = {
                "resultType": "complete",
                "structuredContent": {"from": payload},
            }
            kind = "result"
        if kind == "error":
            reply = {"jsonrpc": "2.0", "id": message.get("id"), "error": payload}
        else:
            reply = {"jsonrpc": "2.0", "id": message.get("id"), "result": payload}
        sys.stdout.write(json.dumps(reply) + "\n")
        sys.stdout.flush()


def serve_http(state, port):
    server = ThreadingHTTPServer(("0.0.0.0", port), Handler)
    server.daemon_threads = True
    server.state = state
    print(f"fake MCP server {state.name} on port {port}", file=sys.stderr, flush=True)
    server.serve_forever()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--transport", choices=("http", "stdio"), required=True)
    parser.add_argument("--port", type=int, default=8001)
    parser.add_argument("--name", default="fake")
    args = parser.parse_args()
    state = State(args.name)
    if args.transport == "http":
        serve_http(state, args.port)
    else:
        serve_stdio(state)


if __name__ == "__main__":
    main()
