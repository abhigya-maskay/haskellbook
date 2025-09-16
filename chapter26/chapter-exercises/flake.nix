{
  description = "Chapter 26 Haskell exercises";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            chapter-exercises = hself.callCabal2nix "chapter-exercises" ./. {};
          };
        };
        
        packageName = "chapter-exercises";
      in {
        packages.${packageName} = haskellPackages.${packageName};
        packages.default = self.packages.${system}.${packageName};

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.${packageName} ];
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            zlib
            pkg-config
          ];
        };
      });
}