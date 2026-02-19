{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskemathesis.url = "github:weyl-ai/haskemathesis";
  };

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { pkgs, system, inputs', ... }:
        let
          hsPkgs = pkgs.haskellPackages.override {
            overrides = self: super: {
              haskemathesis = inputs'.haskemathesis.packages.default;
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
            weapon-server = hsPkgs.weapon-server;
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
              ]);
          };
        };
    };
}
