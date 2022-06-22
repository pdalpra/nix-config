{ pkgs, ... }:

{
  home.packages = with pkgs; [
    onagre
    pop-launcher
  ];
}
