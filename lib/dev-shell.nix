{ pkgs }:

with pkgs;

let
  treefmtAll = writeShellScriptBin "treefmt-all" ''
    treefmt
    treefmt -C .vscode
  '';
  nixTools = [
    cachix
    statix
  ];
  haskellTools = [
    cabal-install
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
    haskellPackages.implicit-hie
    ghc
    ghcid
    haskell-language-server
    hlint
    # FIXME: Needed by dbus, can be improved
  ];
  xmonadDependencies = lib.makeLibraryPath [
    xorg.libX11
    xorg.libXrandr
    xorg.libXScrnSaver
    xorg.libXext
    xorg.libXinerama
    xorg.libXv
    xorg.libXft
    zlib.dev
  ];
  miscTools = [
    git-secret
    jfmt
    treefmt
    treefmtAll
    zlib
  ];
in
mkShell {
  name = "nix-config-dev";

  shellHook = ''
    export LD_LIBRARY_PATH=${xmonadDependencies}:$LD_LIBRARY_PATH
  '';

  buildInputs =
    nixTools ++
    haskellTools ++
    miscTools;
}
