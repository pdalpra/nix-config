{ system, nixpkgs }:
let
  pkgs = nixpkgs.legacyPackages.${system};
  nixTools =
    with pkgs; [
      cachix
      nixpkgs-fmt
      statix
    ];
  haskellTools =
    with pkgs.haskellPackages; [
      cabal-fmt
      fourmolu
      ghcid
      haskell-language-server
      hlint
    ];
  miscTools =
    with pkgs; [
      treefmt
    ];
in
pkgs.mkShell {
  name = "nix-config-dev";
  buildInputs = nixTools ++ haskellTools ++ miscTools;
}
