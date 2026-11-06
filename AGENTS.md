# AGENTS.md

Guidance for agents and humans working in this repository.

## What this repo is

An Emacs Lisp client for the Model Context Protocol (MCP).

| File | Contents |
|---|---|
| `mcp.el` | Public API, connection bootstrap, tool adapters, roots, cancellation |
| `mcp-transport.el` | Connection classes, stdio framing, Streamable HTTP, request headers |
| `mcp-hub.el` | Hub buffer: start servers, browse tools, prompts, resources |
| `test/mcp-test.el` | ERT unit tests: no network, no subprocesses |
| `test/vm/` | Fake MCP server and network scenario driver for the NixOS VM test |
| `Readme.org` | User documentation and configuration examples |

## Protocol target

The target revision is **MCP 2026-07-28**, which is stateless:

- Every request carries `_meta` with `io.modelcontextprotocol/protocolVersion` and
  `io.modelcontextprotocol/clientCapabilities`, and SHOULD carry
  `io.modelcontextprotocol/clientInfo`.
- There is no `initialize` / `notifications/initialized` handshake and no
  `Mcp-Session-Id`.
- `server/discover` reports supported versions, capabilities and server identity.
- Removed methods: `ping`, `logging/setLevel`, `notifications/roots/list_changed`.
  Log level travels per request in `_meta` `io.modelcontextprotocol/logLevel`.
- Multi Round-Trip Requests (MRTR) replace server-initiated requests. Results carry
  `resultType`: `"complete"` or `"input_required"`.
- Streamable HTTP POSTs require `MCP-Protocol-Version`, `Mcp-Method`, and `Mcp-Name`
  where applicable.

Legacy, initialization-based servers are out of scope.

## Build and check

The repo is a Nix flake.

```bash
nix build                 # melpaBuild: byte-compile with warnings as errors
nix flake check           # package, ERT unit tests and the network VM test
nix develop               # Emacs 31.1 plus jsonrpc, curl, plz in PATH
make test                 # ERT unit tests, no network and no subprocesses
make compile              # byte-compile with warnings as errors
git diff --no-ext-diff    # always diff without external tools
```

The network test boots two VMs: one serves `test/vm/fake_server.py` over
Streamable HTTP, the other runs `test/vm/client.el` in batch Emacs over real
sockets and a real stdio subprocess.  It is the only check of the transports
themselves, and it fails on a colliding request id, a lost stream, a noise
line in `*Messages*`, or a protocol violation the fake server reports.

```bash
nix build .#checks.x86_64-linux.mcp-vm --print-build-logs
```

Inside the devShell, a quick compile without Nix:

```bash
emacs -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile mcp.el mcp-hub.el
```

Byte-compilation warnings are errors. Never relax that.

## Conventions

- Emacs Lisp only. Require Emacs 30.1 (`Package-Requires`), with `jsonrpc` and `plz`.
- The nixpkgs Emacs in the devShell is 31.1, so the 30.1 floor is declared but not
  tested. Do not use Emacs 31-only APIs without bumping the header.
- Public API names start with `mcp-`, internal helpers with `mcp--`.
- `mcp-hub.el` names start with `mcp-hub-`.
- Docstrings on every public function and variable. `checkdoc` is the reference.
- Semantic commits: `type(scope): summary`, one concern per commit.
- Transport state belongs to a connection, never to a global key.  JSON-RPC ids
  start at one on every connection, so a table keyed by an id alone makes two
  servers overwrite each other.  `mcp--http-streams` maps connection to id
  table, and `mcp--list-cache` is created per instance.  A class `:initform`
  holding a hash table is evaluated once and shared by every instance.
- Change a transport, a header or a retry, and run the network VM test.  The
  ERT suite stubs the transport and cannot see a wire-level regression.
- `plz` parses the response from the process buffer, so a `:filter` must insert
  the text there as well as read it.  Otherwise plz logs `Error running timer`
  on every request.
- Header values that mirror the JSON body spell JSON scalars: `true`, `false`,
  numbers as digits.  Encode anything plz cannot put in a curl config string.

## TODO

- [ ] Full lint gate in the flake: `checkdoc` plus `package-lint` with dependency
      stubs. Currently only byte-compilation guards the tree.
- [ ] `subscriptions/listen` opt-in stream for server-push change notifications.
- [ ] Report progress notifications (`notifications/progress`) to the user
      instead of only logging them.
- [ ] Cover the hub buffer in the VM test, not just the transport.
