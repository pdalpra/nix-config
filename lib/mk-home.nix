{ myLib
, overlays
, home-manager
, agenix
, catppuccin
}: username: profile: system:

let
  pkgs = overlays system;
in
home-manager.lib.homeManagerConfiguration {
  inherit pkgs;
  extraSpecialArgs = { inherit myLib; };
  modules = [
    ../modules/common/profile.nix
    agenix.homeManagerModules.default
    catppuccin.homeModules.catppuccin
    {
      home.username = username;
      home.homeDirectory = "/home/${username}";
      inherit profile;
    }
    ../home/home.nix
  ];
}
