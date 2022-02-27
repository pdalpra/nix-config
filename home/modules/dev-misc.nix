{ pkgs, ... }:

{
  home.packages = with pkgs; [
    rustup
  ];

  programs.go = {
    enable = true;
    package = pkgs.unstable.go;
    goPath = "Work/go";
  };
}