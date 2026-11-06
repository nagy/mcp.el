{
  description = "Model Context Protocol client and server hub for Emacs (Emacs package)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    # Date of the last commit as YYYY-MM-DD, folded into the version string
    # below.  Flake source info gives it as e.g. "20260922133522", so slice out
    # the year, month and day and glue them with hyphens.
    let
      lastModifiedDate = inputs.self.sourceInfo.lastModifiedDate;
      commitDate = "${builtins.substring 0 4 lastModifiedDate}-${
        builtins.substring 4 2 lastModifiedDate
      }-${builtins.substring 6 2 lastModifiedDate}";
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      # Elisp is architecture independent, so there is no cost in evaluating
      # both Linux arches.
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          pkgs,
          lib,
          config,
          ...
        }:
        let
          inherit (pkgs.emacsPackages) melpaBuild;
          # Elisp dependencies from the Package-Requires header that nixpkgs
          # already packages.  The header asks for jsonrpc 1.0.25 and nixpkgs
          # ships 1.0.29, which satisfies it.
          emacsDeps = with pkgs.emacsPackages; [ jsonrpc ];
          # Emacs with the dependencies on EMACSLOADPATH, for batch test runs.
          emacs = pkgs.emacsPackages.emacsWithPackages (_: emacsDeps);
          # Byte-compiled leftovers from a local session must never shadow the
          # sources the tests load.
          src = lib.cleanSourceWith {
            src = ./.;
            filter = path: type: !(lib.hasSuffix ".elc" path);
          };
        in
        {
          packages.mcp = melpaBuild {
            pname = "mcp";
            # Nix rejects versions it cannot parse.  Convention for unreleased
            # packages: <upstream-version>-unstable-<YYYY-MM-DD>.  Derived from
            # the flake's own source info, so it never goes stale.
            version = "0.1.0-unstable-${commitDate}";

            src = lib.cleanSource ./.;

            packageRequires = emacsDeps;

            # Byte-compilation warnings fail the build.  Keep it on: with no
            # ERT suite yet, this is the only automated gate the package has.
            # lint gate tracked in AGENTS.md.
            turnCompilationWarningToError = true;

            meta = {
              description = "Emacs client and server hub for the Model Context Protocol";
              longDescription = ''
                Connects Emacs to Model Context Protocol servers, locally over
                stdio and remotely over Streamable HTTP.  Provides the low-level
                jsonrpc-based client in mcp.el, gptel-compatible tool adapters,
                and mcp-hub.el, a buffer for starting servers and browsing the
                tools, prompts and resources they expose.

                Targets the stateless 2026-07-28 protocol revision, where every
                request carries its protocol version and client capabilities
                instead of relying on an initialize handshake.
              '';
              # GNU General Public License v3, per the file headers.
              license = lib.licenses.gpl3Plus;
              homepage = "https://github.com/lizqwerscott/mcp.el";
              maintainers = with lib.maintainers; [ ];
              # mcp.el runs Emacs 30.1 or later on any platform Emacs supports.
              # The nixpkgs Emacs in the devShell is newer than that floor.
              platforms = lib.platforms.unix;
            };
          };

          packages.default = config.packages.mcp;

          # Unit tests: request metadata, HTTP body readers, MRTR helpers.
          # No network access and no subprocesses.
          checks.ert = pkgs.runCommand "mcp-ert" {
            nativeBuildInputs = [ emacs ];
            inherit src;
          } ''
            export HOME=$TMPDIR
            emacs --batch --eval "(require 'package)" -f package-initialize \
              -L "$src" -L "$src/test" -l ert -l mcp-test.el \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';

          devShells.default = pkgs.mkShell {
            # Emacs is 31.1 here while the package header keeps a 30.1 floor:
            # nixpkgs-unstable no longer carries emacs30.
            packages = [ pkgs.emacs ] ++ emacsDeps;
          };
        };
    };
}
