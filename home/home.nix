{ lib, myLib, pkgs, ... }:

with lib;
with myLib;

let
  importAll = imports: foldl' concat [ ] (map import imports);
  generic = [
    ./modules/cli
    ./modules/dev
    ./modules/editors
    #   ./modules/media
    ./modules/misc
    ./modules/web
  ];
  linuxSpecific = [ ]; # mkOptional pkgs.stdenv.isLinux [ ./modules/wm ] [ ];
in
{
  imports = importAll (generic ++ linuxSpecific);

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  xdg.userDirs = mkIf pkgs.stdenv.isLinux {
    enable = true;
    createDirectories = true;
  };

  news.display = "silent";

  home.stateVersion = "22.11";
}
