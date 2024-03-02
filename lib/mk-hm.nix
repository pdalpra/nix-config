username: { overlays, home-manager, system }:

let
  pkgs = overlays system;
  isDarwin = pkgs.lib.strings.hasSuffix system "-darwin";
  extraSpecialArgs = { inherit isDarwin; };
  homeDirectory = if isDarwin then "/Users/${username}" else "/home/${username}";
in
home-manager.lib.homeManagerConfiguration {
  inherit pkgs extraSpecialArgs;

  modules = [
    { home = { inherit username homeDirectory; }; }
    ../home/home.nix
  ];
}
