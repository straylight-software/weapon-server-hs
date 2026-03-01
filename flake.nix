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
      ];

      flake = {
        nixosModules.default = import ./nix/module.nix;
        nixosModules.weapon-server = import ./nix/module.nix;
      };

      perSystem =
        {
          pkgs,
          inputs',
          ...
        }:
        let
          # Get pre-built haskemathesis packages from the flake
          # These are built with the same nixpkgs (via inputs.nixpkgs.follows)
          haskemathesisPkgs = inputs'.haskemathesis.packages;

          # Falsify fork source from haskemathesis inputs
          falsifySrc = inputs.haskemathesis.inputs.falsify.outPath + "/lib";

          # Enable profiling for a package
          withProfiling =
            pkg:
            pkgs.haskell.lib.enableExecutableProfiling (pkgs.haskell.lib.enableLibraryProfiling pkg);

          hsPkgs = pkgs.haskellPackages.override {
            overrides = self: super:
              let
                # Rebuild a haskemathesis package with profiling using our haskellPackages
                # We call the cabal2nix expression and override src via overrideCabal
                rebuildWithProfiling =
                  origPkg:
                  withProfiling (
                    pkgs.haskell.lib.overrideCabal (self.callPackage origPkg.passthru.cabal2nixDeriver { }) (
                      old: {
                        src = origPkg.src;
                      }
                    )
                  );
              in
              {
                # Falsify fork with profiling (required by haskemathesis)
                falsify = withProfiling (self.callCabal2nix "falsify" falsifySrc { });
                # Rebuild haskemathesis packages with our haskellPackages and profiling
                haskemathesis-core = rebuildWithProfiling haskemathesisPkgs.haskemathesis-core;
                haskemathesis = rebuildWithProfiling haskemathesisPkgs.haskemathesis;
                haskemathesis-tasty = rebuildWithProfiling haskemathesisPkgs.haskemathesis-tasty;
                weapon-server = self.callPackage ./default.nix { };
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
        in
        {
          packages = {
            default = server;
            inherit (hsPkgs) weapon-server;
            docs = hsPkgs.weapon-server.doc;
          };

          checks.weapon-server = server;

          devShells.default = hsPkgs.shellFor {
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
        };
    };
}
