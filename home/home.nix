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
    ./modules/misc
    ./modules/web
    ./modules/wm
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

  home.stateVersion = "22.11";
}
