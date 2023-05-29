username: { nixpkgs, nixpkgs-unstable, nurpkgs, home-manager, system }:

let
  allowUnfree = true;
  config = { allowUnfree = true; };
  isDarwin = nixpkgs.lib.strings.hasSuffix system "-darwin";
  homeDirectory = if isDarwin then "/Users/${username}" else "/home/${username}";
  pkgsForNur = import nixpkgs { inherit config system; };
  nurOverlay = final: prev: {
    nur = import nurpkgs {
      pkgs = pkgsForNur;
      nurpkgs = pkgsForNur;
    };
  };
  unstableOverlay = final: prev: {
    unstable = import nixpkgs-unstable { inherit config system; };
  };
  pkgs = import nixpkgs {
    inherit config system;

    overlays = [
      nurOverlay
      unstableOverlay
    ];
  };
  base = {
    home = { inherit username homeDirectory; };
  };
in
home-manager.lib.homeManagerConfiguration rec {
  inherit pkgs;

  modules = [
    (base)
    ../home/home.nix
  ];
}
