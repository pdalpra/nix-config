{ pkgs, lintingPkgs, agenixBin }:

let
  agenixFile = pkgs.writeShellScriptBin "agenix-file" "cat $2 | ${agenixBin}/bin/agenix --editor='-' -e $1";
  nixTools = with pkgs; [
    cachix
    git-secret
    nixpkgs-fmt
    unstable.nixd
    agenixBin
    agenixFile
  ];
  haskellTools = with pkgs.haskellPackages; [
    cabal-install
    fourmolu
    haskell-language-server
    hlint
  ];
  xmonadDependencies = with pkgs; [
    xorg.libX11
    xorg.libXrandr
    xorg.libXScrnSaver
    xorg.libXext
    xorg.libXinerama
    zlib
    zlib.dev
  ];
in
pkgs.mkShell {
  name = "nix-config-dev";

  buildInputs =
    lintingPkgs ++
    nixTools ++
    haskellTools ++
    xmonadDependencies;
}
