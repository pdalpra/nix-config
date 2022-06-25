{ system, nixpkgs }:
let
  pkgs = nixpkgs.legacyPackages.${system};
  haskellPackages = pkgs.haskellPackages;
  xmonadDeps = (import ./home/modules/xmonad-config/packages.nix { inherit haskellPackages; });
in
pkgs.mkShell {
  name = "nix-config-dev";
  buildInputs = with pkgs; [
    cachix
    nix-linter
    haskellPackages.fourmolu
  ] ++ xmonadDeps;
}
