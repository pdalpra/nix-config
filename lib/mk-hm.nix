username: { overlays, home-manager, system }:

let
  pkgs = overlays system;
  isDarwin = pkgs.lib.strings.hasSuffix system "-darwin";
  specialArgs = {
    inherit isDarwin;
    myLib = import ./utils.nix { inherit (pkgs) lib; };
  };
  homeDirectory = if isDarwin then "/Users/${username}" else "/home/${username}";
in
home-manager.lib.homeManagerConfiguration {
  inherit pkgs;

  extraSpecialArgs = specialArgs;

  modules = [
    { home = { inherit username homeDirectory; }; }
    ../home/home.nix
  ];
}
