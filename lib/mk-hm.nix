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
in
home-manager.lib.homeManagerConfiguration rec {
  inherit system pkgs username homeDirectory;

  stateVersion = "21.11";

  configuration.imports = [
    ../home/home.nix
  ];
}
