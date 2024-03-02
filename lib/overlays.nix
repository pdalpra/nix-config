{ nixpkgs, nixpkgs-unstable, nurpkgs }: system:

let
  config = { allowUnfree = true; };
  pkgs = import nixpkgs { inherit config system; };
  nurOverlay = final: prev: {
    nur = import nurpkgs {
      inherit pkgs;
      nurpkgs = pkgs;
    };
  };
  unstableOverlay = final: prev: {
    unstable = import nixpkgs-unstable { inherit config system; };
  };
  withCustomLib = final: prev: {
    lib = prev.lib // {
      my = import ./utils.nix { inherit (prev) lib; };
    };
  };
in
import nixpkgs {
  inherit config system;

  overlays = [
    nurOverlay
    unstableOverlay
    withCustomLib
  ];
}
