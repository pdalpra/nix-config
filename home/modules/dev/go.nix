{ pkgs, ... }:

{
  programs.go = {
    enable = true;
    package = pkgs.unstable.go;
    goPath = "Work/go";
  };
}
