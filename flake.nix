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
      url = "git+ssh://git@github.com/straylight-software/haskemathesis.git?ref=executor-io-api";
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
          hsPkgs = pkgs.haskellPackages.override {
            overrides = self: _super: {
              haskemathesis = pkgs.haskell.lib.enableLibraryProfiling inputs'.haskemathesis.packages.default;
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
