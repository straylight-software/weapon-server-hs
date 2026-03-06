{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    haskemathesis = {
      url = "git+ssh://git@github.com/straylight-software/haskemathesis.git";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    weapon = {
      url = "git+ssh://git@github.com/straylight-software/weapon.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Sensenet - Buck2 build system integration
    sensenet = {
      url = "git+ssh://git@github.com/straylight-software/sensenet?ref=feat/nativelink-isa-gcp";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      imports = [
        ./nix/formatter.nix
        ./nix/ui.nix
        inputs.sensenet.flakeModules.sensenet
      ];

      flake = {
        nixosModules.default = import ./nix/module.nix;
        nixosModules.weapon-server = import ./nix/module.nix;
      };

      perSystem =
        {
          pkgs,
          inputs',
          config,
          ...
        }:
        let
          # Build parquet-ffi (Rust library for Parquet support)
          parquet-ffi = pkgs.callPackage ./parquet-ffi { };

          # Get pre-built haskemathesis packages from the flake
          # These are built with the same nixpkgs (via inputs.nixpkgs.follows)
          haskemathesisPkgs = inputs'.haskemathesis.packages;

          # Falsify fork source from haskemathesis inputs
          falsifySrc = inputs.haskemathesis.inputs.falsify.outPath + "/lib";

          # Enable profiling for a package
          withProfiling =
            pkg: pkgs.haskell.lib.enableExecutableProfiling (pkgs.haskell.lib.enableLibraryProfiling pkg);

          hsPkgs = pkgs.haskellPackages.override {
            overrides =
              self: _super:
              let
                # Rebuild a haskemathesis package with profiling using our haskellPackages
                # We call the cabal2nix expression and override src via overrideCabal
                rebuildWithProfiling =
                  origPkg:
                  withProfiling (
                    pkgs.haskell.lib.overrideCabal (self.callPackage origPkg.passthru.cabal2nixDeriver { }) (_old: {
                      inherit (origPkg) src;
                    })
                  );
              in
              {
                # Falsify fork with profiling (required by haskemathesis)
                falsify = withProfiling (self.callCabal2nix "falsify" falsifySrc { });
                # Rebuild haskemathesis packages with our haskellPackages and profiling
                haskemathesis-core = rebuildWithProfiling haskemathesisPkgs.haskemathesis-core;
                haskemathesis = rebuildWithProfiling haskemathesisPkgs.haskemathesis;
                haskemathesis-tasty = rebuildWithProfiling haskemathesisPkgs.haskemathesis-tasty;
                weapon-server = self.callPackage ./default.nix { inherit parquet-ffi; };
              };
          };

          runtimePkgs = with pkgs; [
            ripgrep
            git
            fd
          ];

          server = pkgs.writeShellApplication {
            name = "weapon-server";
            runtimeInputs = runtimePkgs;
            text = ''
              exec ${hsPkgs.weapon-server}/bin/weapon-server "$@"
            '';
          };
          # GHC 9.10 with packages for Buck2 builds
          inherit (pkgs.haskell.packages) ghc910;

        in
        {
          # ════════════════════════════════════════════════════════════════════════
          # Packages
          # ════════════════════════════════════════════════════════════════════════
          #
          # nix build        -> cabal-built weapon-server (hermetic)
          # nix build .#sensenet-weapon-server -> buck2-built (requires --impure)
          #
          packages = {
            default = server;
            inherit (hsPkgs) weapon-server;
            docs = hsPkgs.weapon-server.doc;
            inherit parquet-ffi;
          };

          # ════════════════════════════════════════════════════════════════════════
          # Checks (nix flake check)
          # ════════════════════════════════════════════════════════════════════════
          #
          # Run the Haskell test suite via cabal (hermetic).
          # Buck2 tests require devshell: nix develop .#sensenet-weapon-server -c buck2 test //:test
          #
          checks = {
            # Build check - ensures the package builds
            inherit (hsPkgs) weapon-server;

            # Test check - disabled until test suite issues are fixed
            # (missing defaultsPath, defaultTelemetry exports)
            # weapon-server-tests = pkgs.haskell.lib.overrideCabal hsPkgs.weapon-server (_old: {
            #   doCheck = true;
            # });
          };

          # ════════════════════════════════════════════════════════════════════════
          # DevShells
          # ════════════════════════════════════════════════════════════════════════
          #
          # nix develop                      -> sensenet-weapon-server (buck2)
          # nix develop .#cabal              -> cabal-based shell
          # nix develop .#sensenet-weapon-server -> same as default
          #
          # Note: devShells.default is set below after sensenet.projects defines
          # sensenet-weapon-server. The sensenet module auto-creates devShells.sensenet-weapon-server.
          #
          devShells.cabal = hsPkgs.shellFor {
            packages = p: [ p.weapon-server ];
            buildInputs =
              runtimePkgs
              ++ (with pkgs; [
                cabal-install
                ghcid
                haskell-language-server
                haskellPackages.stan
                liburing
                mdbook
              ]);
          };

          # ════════════════════════════════════════════════════════════════════════
          # Sensenet (Buck2) project configuration
          # ════════════════════════════════════════════════════════════════════════
          sensenet.projects.weapon-server = {
            src = ./.;
            targets = [
              "//:weapon-server"
              "//:test"
            ];
            toolchain = {
              cxx.enable = true;
              haskell = {
                enable = true;
                ghcpackages = ghc910;
                packages = hp: [
                  # Core dependencies from weapon-server.cabal
                  hp.aeson
                  hp.async
                  hp.base
                  hp.base16-bytestring
                  hp.base64-bytestring
                  hp.bytestring
                  hp.case-insensitive
                  hp.containers
                  hp.crypton
                  hp.crypton-connection
                  hp.data-default-class
                  hp.dhall
                  hp.dhall-json
                  hp.directory
                  hp.filepath
                  hp.http-api-data
                  hp.http-client
                  hp.http-client-tls
                  hp.http-types
                  hp.katip
                  hp.memory
                  hp.mtl
                  hp.network
                  hp.optparse-applicative
                  hp.posix-pty
                  hp.primitive
                  hp.process
                  hp.random
                  hp.servant
                  hp.servant-server
                  hp.stm
                  hp.text
                  hp.time
                  hp.tls
                  hp.unix
                  hp.unliftio-core
                  hp.vault
                  hp.vector
                  hp.wai
                  hp.wai-extra
                  hp.wai-websockets
                  hp.warp
                  hp.websockets
                  # Test dependencies
                  hp.hedgehog
                  hp.hspec
                  hp.hspec-core
                  hp.hspec-hedgehog
                  hp.openapi3
                  hp.regex-pcre
                  hp.tasty
                  hp.tasty-hedgehog
                  hp.tasty-hspec
                  hp.temporary
                  hp.transformers
                  hp.unordered-containers
                ];
              };
            };
            # Extra buckconfig sections for io_uring and parquet_ffi library paths
            extrabuckconfigsections = ''

              [io-uring]
              liburing_lib = ${pkgs.liburing}/lib
              liburing_include = ${pkgs.liburing.dev}/include

              [parquet-ffi]
              parquet_ffi_lib = ${parquet-ffi}/lib
              parquet_ffi_include = ${parquet-ffi}/include
            '';
            devshellpackages = [
              pkgs.cabal-install
              pkgs.ghcid
              ghc910.haskell-language-server
              pkgs.liburing
              pkgs.mdbook
              parquet-ffi
            ];
          };

          # Set default devShell to the sensenet one (with buck2)
          devShells.default = config.devShells.sensenet-weapon-server;
        };
    };
}
