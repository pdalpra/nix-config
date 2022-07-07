{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  name = "nix-config-dev";
  buildInputs = with pkgs; [
    cachix
    statix
  ];
}
