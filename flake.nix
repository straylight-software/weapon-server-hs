{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    haskemathesis.url = "git+ssh://git@github.com/straylight-software/haskemathesis.git";
    treefmt-nix.url = "github:numtide/treefmt-nix";
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
      ];

      perSystem =
        { pkgs, inputs', ... }:
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
                liburing # io_uring bindings
              ]);
          };
        };
    };
}
