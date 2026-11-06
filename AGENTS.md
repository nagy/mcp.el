# AGENTS.md

Guidance for agents and humans working in this repository.

## What this repo is

An Emacs Lisp client for the Model Context Protocol (MCP).

| File | Contents |
|---|---|
| `mcp.el` | Connection objects, public API, tool adapters, roots |
| `mcp-transport.el` | stdio framing and Streamable HTTP transport (planned, see roadmap) |
| `mcp-hub.el` | Hub buffer: start servers, browse tools, prompts, resources |
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
nix flake check           # evaluates packages and checks
nix develop               # Emacs 31.1 plus jsonrpc, curl, plz in PATH
git diff --no-ext-diff    # always diff without external tools
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

## TODO

- [ ] Add ERT tests and a `checks.ert` derivation (unit tests only, no network).
- [ ] Full lint gate in the flake: `checkdoc` plus `package-lint` with dependency
      stubs. Currently only byte-compilation guards the tree.
- [ ] `mcp-transport.el`: move stdio framing and Streamable HTTP out of `mcp.el`.
- [ ] Replace the handshake with `server/discover` bootstrap.
- [ ] Streamable HTTP over `plz` with an incremental SSE parser.
- [ ] MRTR support: `input_required` results and `inputResponses` retries.
- [ ] Pagination plus `ttlMs` / `cacheScope` caching.
- [ ] `subscriptions/listen` opt-in stream.
- [ ] Remove stale helpers left over from the deprecated HTTP+SSE transport.
