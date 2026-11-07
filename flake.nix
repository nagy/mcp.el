{
  description = "Model Context Protocol client and server hub for Emacs (Emacs package)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # Formatter driver behind `nix fmt`.  Its nixpkgs must follow ours, or
    # the formatters run against an interpreter we do not build with.
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      flake-parts,
      treefmt-nix,
      ...
    }:
    # Date of the last commit as YYYY-MM-DD, folded into the version string
    # below.  Flake source info gives it as e.g. "20260922133522", so slice out
    # the year, month and day and glue them with hyphens.  A tree without
    # commits has no date, so fall back to the epoch.
    let
      stamp =
        let
          date = inputs.self.sourceInfo.lastModifiedDate;
        in
        if date == null then "19700101000000" else date;
      commitDate = "${builtins.substring 0 4 stamp}-${builtins.substring 4 2 stamp}-${builtins.substring 6 2 stamp}";
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      # Adds `checks.treefmt` and the `nix fmt` app.
      imports = [ treefmt-nix.flakeModule ];

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
          # Elisp dependencies from the Package-Requires header that nixpkgs
          # already packages.
          emacsDeps = with pkgs.emacsPackages; [
            jsonrpc
            plz
          ];
          # Emacs with the dependencies on EMACSLOADPATH, for batch test runs
          # and for the VM test.  The package builds with its own set instead.
          emacs = pkgs.emacsPackages.emacsWithPackages (_: emacsDeps);
          # Byte-compiled leftovers from a local session must never shadow the
          # sources the tests load.
          src = lib.cleanSourceWith {
            src = ./.;
            filter = path: _type: !(lib.hasSuffix ".elc" path);
          };

          # The package stays overridable, so a consumer can swap the Emacs,
          # the dependency set or the source:
          #   inputs.mcp.packages.${system}.mcp.override {
          #     emacs = pkgs.emacs-nox;
          #   }
          # The flake passes the commit-derived version, because a plain
          # import of the expression cannot know it.
          mcp = pkgs.lib.makeOverridable (
            {
              # One Emacs package set supplies every dependency, so an
              # override of this argument moves all of them.
              emacs ? pkgs.emacs,
              # `melpaBuild' closes over the Emacs of its package set, so an
              # `emacs' argument on the build itself does nothing.  Derive
              # the set from the chosen Emacs instead.
              emacsPackages ? pkgs.emacsPackagesFor emacs,
              melpaBuild ? emacsPackages.melpaBuild,
              # Require this list to move with the package set: a dependency
              # built for another Emacs fails to load at byte-compile time.
              packageRequires ? with emacsPackages; [
                jsonrpc
                plz
              ],
              version ? "0.1.0",
              # A fork may build from another tree.  The default drops VCS
              # metadata and stale byte-code.
              packageSrc ? src,
            }:
            melpaBuild {
              pname = "mcp";
              inherit version packageRequires;
              src = packageSrc;

              # Byte-compilation warnings fail the build.
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
            }
          ) { version = "0.1.0-unstable-${commitDate}"; };
        in
        {
          packages = {
            inherit mcp;
            default = mcp;
          };

          # Unit tests: request metadata, HTTP body readers, MRTR helpers.
          # No network access and no subprocesses.
          checks.ert =
            pkgs.runCommand "mcp-ert"
              {
                nativeBuildInputs = [ emacs ];
                inherit src;
              }
              ''
                export HOME=$TMPDIR
                emacs --batch --eval "(require 'package)" -f package-initialize \
                  -L "$src" -L "$src/test" -l ert -l mcp-test.el \
                  -f ert-run-tests-batch-and-exit
                touch $out
              '';

          # Network test: two VMs, real sockets, real subprocesses.  The ERT
          # suite can only stub the transport, so this is the check that sees
          # a broken HTTP reader, a colliding request id or a stray timer
          # error.  The client node runs test/vm/client.el against the fake
          # server in test/vm/fake_server.py.
          checks.mcp-vm = pkgs.testers.runNixOSTest {
            name = "mcp-vm";

            nodes.server =
              { pkgs, ... }:
              let
                fakeServer = "${pkgs.python3}/bin/python3 ${./test/vm/fake_server.py}";
                service = name: port: {
                  wantedBy = [ "multi-user.target" ];
                  after = [ "network.target" ];
                  serviceConfig = {
                    ExecStart = "${fakeServer} --transport http --port ${toString port} --name ${name}";
                    Restart = "always";
                  };
                };
              in
              {
                networking.firewall.enable = false;
                systemd.services.mcp-fake-a = service "a" 8001;
                systemd.services.mcp-fake-b = service "b" 8002;
              };

            nodes.client = {
              environment.systemPackages = [
                emacs
                pkgs.curl
                pkgs.python3
              ];
              environment.etc."mcp-src".source = src;
              environment.variables = {
                MCP_VM_URL_A = "http://server:8001/mcp";
                MCP_VM_URL_B = "http://server:8002/mcp";
                MCP_VM_STATS_A = "http://server:8001/stats";
                MCP_VM_STATS_B = "http://server:8002/stats";
                MCP_VM_PYTHON = "${pkgs.python3}/bin/python3";
                MCP_VM_FAKE = "${./test/vm/fake_server.py}";
                MCP_VM_ROOT = "/tmp/mcp-vm-root";
              };
            };

            testScript = ''
              start_all()
              server.wait_for_unit("mcp-fake-a.service")
              server.wait_for_unit("mcp-fake-b.service")
              server.wait_for_open_port(8001)
              server.wait_for_open_port(8002)
              client.succeed("mkdir -p /tmp/mcp-vm-root")
              out = client.succeed(
                  "emacs --batch -L /etc/mcp-src"
                  " -l /etc/mcp-src/test/vm/client.el"
              )
              print(out)
              assert "PASS: every network check held" in out, out
            '';
          };

          # One formatter set for the tree.  `nix flake check` runs the same
          # treefmt in check mode, so a commit cannot leave it unformatted.
          treefmt.programs.nixfmt.enable = true;

          devShells.default = pkgs.mkShell {
            # Emacs is 31.1 here while the package header keeps a 30.1 floor:
            # nixpkgs-unstable no longer carries emacs30.
            packages = [
              emacs
              pkgs.curl
              # `treefmt` in the shell, so formatting works without Nix
              # evaluation on every run.
              config.treefmt.build.wrapper
            ];
          };
        };
    };
}
