{ pkgs }:

with pkgs;

let
  fmtAll = writeShellScriptBin "fmt-all" "treefmt .github .";
in
mkShell {
  name = "nix-config-dev";

  buildInputs = [
    cachix
    git-secret
    fmtAll
    statix
    treefmt
    nixpkgs-fmt
    nil
    yamlfix
  ];
}
