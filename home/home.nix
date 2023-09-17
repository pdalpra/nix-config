{ lib, ... }:

with lib;

let
  importAll = imports: foldl' concat [ ] (map import imports);
in
{
  imports = importAll [
    ./modules/cli
    ./modules/dev
    ./modules/editors
    ./modules/media
    ./modules/web
    ./modules/wm
  ] ++ [
    ./modules/fonts.nix
    ./modules/security.nix
  ];

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  xdg.userDirs =
    {
      enable = true;
      createDirectories = true;
    };

  news.display = "silent";

  # After initial install, let home-manager manage itself
  programs.home-manager.enable = true;

  home.stateVersion = "22.11";
}
