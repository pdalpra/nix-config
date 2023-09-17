username: { nixpkgs, nixpkgs-unstable, nurpkgs, home-manager, system }:

let
  specialArgs = {
    my-utils = import ./utils.nix { inherit (nixpkgs) lib; };
  };
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
home-manager.lib.homeManagerConfiguration {
  inherit pkgs;

  extraSpecialArgs = specialArgs;

  modules = [
    { home = { inherit username homeDirectory; }; }
    ../home/home.nix
  ];
}
