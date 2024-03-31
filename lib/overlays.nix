{ nixpkgs, nixpkgs-unstable, nurpkgs }: system:

let
  config = { allowUnfree = true; };
  pkgs = import nixpkgs { inherit config system; };
  utils = import ./utils.nix { inherit (pkgs) lib; };
  nurOverlay = _: _: {
    nur = import nurpkgs {
      inherit pkgs;
      nurpkgs = pkgs;
    };
  };
  unstableOverlay = _: _: {
    unstable = import nixpkgs-unstable { inherit config system; };
  };
  #customLib = final: prev: {
  #  final.lib = prev.lib // utils;
  #};
in
import nixpkgs {
  inherit config system;

  overlays = [
    #customLib
    nurOverlay
    unstableOverlay
  ];
}

