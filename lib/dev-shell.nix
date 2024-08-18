{ pkgs, lintingPkgs, agenixBin }:

with pkgs;

let
  agenixFile = writeShellScriptBin "agenix-file" "cat $2 | ${agenixBin}/bin/agenix --editor='-' -e $1";
  nixTools = [
    cachix
    git-secret
    nixpkgs-fmt
    unstable.nixd
    agenixBin
    agenixFile
  ];
  haskellTools = with haskellPackages; [
    cabal-install
    fourmolu
    haskell-language-server
    hlint
  ];
  xmonadDependencies = [
    xorg.libX11
    xorg.libXrandr
    xorg.libXScrnSaver
    xorg.libXext
    xorg.libXinerama
    zlib
    zlib.dev
  ];
in
mkShell {
  name = "nix-config-dev";

  buildInputs =
    lintingPkgs ++
    nixTools ++
    haskellTools ++
    xmonadDependencies;
}
