{ pkgs }:

with pkgs;

mkShell {
  name = "nix-config-dev";
  buildInputs = [
    cachix
    git-secret
    statix
  ];
}
