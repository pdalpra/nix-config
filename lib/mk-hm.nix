{ myLib
, overlays
, home-manager
, agenix
, system
}: username:

let
  pkgs = overlays system;
  isDarwin = pkgs.lib.strings.hasSuffix system "-darwin";
  specialArgs = { inherit isDarwin myLib; };
  homeDirectory = if isDarwin then "/Users/${username}" else "/home/${username}";
in
home-manager.lib.homeManagerConfiguration {
  inherit pkgs;

  extraSpecialArgs = specialArgs;

  modules = [
    { home = { inherit username homeDirectory; }; }
    agenix.homeManagerModules.default
    ../home/home.nix
  ];
}
