{ pkgs, unstable }:

with pkgs;

let
  sourcePaths = builtins.concatStringsSep " " [ "." ".github" ];
  fmtAll = writeShellScriptBin "fmt-all" "${treefmt}/bin/treefmt ${sourcePaths}";
  lintAll = writeShellScriptBin "lint-all" ''
    ${treefmt}/bin/treefmt --fail-on-change --no-cache ${sourcePaths};
    statix check
  '';
  nixTools = [
    cachix
    git-secret
    nixpkgs-fmt
    unstable.nil
    statix
  ];
  haskellTools = with haskellPackages; [
    cabal-install
    cabal-fmt
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
  miscTools = [
    git-secret
    fmtAll
    lintAll
    yamlfix
  ];
in
mkShell {
  name = "nix-config-dev";

  buildInputs =
    nixTools ++
    haskellTools ++
    miscTools ++
    xmonadDependencies;
}
