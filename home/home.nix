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
  ];

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  news.display = "silent";

  # After initial install, let home-manager manage itself
  programs.home-manager.enable = true;

  home.stateVersion = "22.11";
}
