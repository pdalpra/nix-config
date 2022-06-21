username: { nixpkgs, nixpkgs-unstable, home-manager, system }:

let
  allowUnfree = true;
  config = { allowUnfree = true; };
  isDarwin = nixpkgs.lib.strings.hasSuffix system "-darwin";
  homeDirectory = if isDarwin then "/Users/${username}" else "/home/${username}";
  unstable-overlay = final: prev: {
    unstable = import nixpkgs-unstable { inherit config system; };
  };
  pkgs = import nixpkgs {
    inherit config system;

    overlays = [
      unstable-overlay
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
