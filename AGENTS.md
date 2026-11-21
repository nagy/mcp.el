# AGENTS.md — mcp.el

## Purpose

`mcp.el`: Emacs Lisp client for the Model Context Protocol (MCP). Emacs 30.1 or
newer in `Package-Requires`, with `jsonrpc` and `plz`. License: AGPL-3.0-or-later.

## Layout

| File | Contents |
|---|---|
| `mcp.el` | Public API, connection bootstrap, tool adapters, roots, cancellation. |
| `mcp-transport.el` | Connection classes, stdio framing, Streamable HTTP, request headers. |
| `mcp-hub.el` | Hub buffer: start servers, browse tools, prompts, resources. |
| `test/mcp-test.el` | ERT unit tests. No network, no subprocess. |
| `test/vm/` | Fake MCP server and network scenario driver for the NixOS VM test. |
| `Readme.org` | User documentation and configuration examples. |
| `flake.nix` | `melpaBuild` package, checks, the VM test, devShell, treefmt. |

## Build and test

- `nix flake check` — the gate. Run before a commit.
- `nix build` — the package.
- `make test` — ERT unit tests, no network and no subprocess.
- `make compile` — byte-compile only.
- `nix fmt` — nixfmt through treefmt. Run it before a commit.
- `nix develop` — Emacs 31.1 plus jsonrpc, curl and plz on `PATH`.
- `nix build .#checks.x86_64-linux.mcp-vm --print-build-logs` — the network test
  alone.
- `git diff --no-ext-diff` — always diff without external tools.

The network test boots two VMs. One serves `test/vm/fake_server.py` over Streamable
HTTP, the other runs `test/vm/client.el` in batch Emacs over real sockets and a
real stdio subprocess. It is the only check of the transports themselves. It
fails on a colliding request id, a lost stream, a noise line in `*Messages*`, or a
protocol violation that the fake server reports.

A quick compile without Nix, inside the devShell:
`emacs -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f
batch-byte-compile mcp.el mcp-hub.el`.

## Architecture

### Protocol target

The target revision is MCP 2026-07-28, which is stateless. Legacy,
initialization-based servers stay out of scope.

- Every request carries `_meta` with
  `io.modelcontextprotocol/protocolVersion` and
  `io.modelcontextprotocol/clientCapabilities`, and should carry
  `io.modelcontextprotocol/clientInfo`.
- No `initialize` / `notifications/initialized` handshake and no
  `Mcp-Session-Id`.
- `server/discover` reports supported versions, capabilities and server
  identity.
- Removed methods: `ping`, `logging/setLevel`,
  `notifications/roots/list_changed`. Log level travels per request in the `_meta`
  key `io.modelcontextprotocol/logLevel`.
- Multi Round-Trip Requests (MRTR) replace server-initiated requests. A result
  carries `resultType`: `"complete"` or `"input_required"`.
- Streamable HTTP POSTs require `MCP-Protocol-Version`, `Mcp-Method`, and
  `Mcp-Name` where applicable.

### Transport state

Transport state belongs to a connection, never to a global key. JSON-RPC ids start
at one on every connection, so a table keyed by an id alone makes two servers
overwrite each other. `mcp--http-streams` maps a connection to an id table, and
`mcp--list-cache` is created per instance. A class `:initform` holding a hash
table is evaluated once and shared by every instance.

## Rules

- Never relax `byte-compile-error-on-warn`. A relaxed gate ships the defect.
- Change a transport, a header or a retry, and run the network VM test. The ERT
  suite stubs the transport and cannot see a wire-level regression.
- Insert the response text into the process buffer from a `:filter`. `plz` parses
  the response there, so a filter that only reads the text makes plz log
  `Error running timer` on every request.
- Header values that mirror the JSON body spell JSON scalars: `true`, `false`,
  numbers as digits. Encode anything plz cannot put in a curl config string.
- Do not run a formatter alone. The options live in the flake only.
- Do not use Emacs 31-only APIs without bumping the `Package-Requires` floor. The
  nixpkgs Emacs in the devShell is 31.1, so the 30.1 floor is declared but not
  tested.

## Conventions

- Emacs Lisp only.
- Public API names start with `mcp-`, internal helpers with `mcp--`. Names in
  `mcp-hub.el` start with `mcp-hub-`.
- Docstrings on every public function and variable. checkdoc is the reference.
- Elisp and Python stay unformatted by treefmt. Only `nixfmt` runs.
- Semantic commits: `type(scope): summary`, one concern per commit.

## Open work

- Full lint gate in the flake: checkdoc plus package-lint with dependency stubs.
  Only byte-compilation guards the tree today.
- `subscriptions/listen` opt-in stream for server-push change notifications.
- Report progress notifications (`notifications/progress`) to the user instead of
  only logging them.
- Cover the hub buffer in the VM test, not just the transport.
