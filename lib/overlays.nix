{ nixpkgs, nixpkgs-unstable, nurpkgs }: system:

let
  config = { allowUnfree = true; };
  pkgs = import nixpkgs { inherit config system; };
  nurOverlay = _: _: {
    nur = import nurpkgs {
      inherit pkgs;
      nurpkgs = pkgs;
    };
  };
  unstableOverlay = _: _: {
    unstable = import nixpkgs-unstable { inherit config system; };
  };
  customLib = _: prev: {
    lib = prev.lib // {
      extra = import ./utils.nix { inherit (prev) lib; };
    };
  };
in
import nixpkgs {
  inherit config system;

  overlays = [
    nurOverlay
    unstableOverlay
    customLib
  ];
}

