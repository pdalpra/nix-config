{ pkgs, ... }:

{
  home.packages = with pkgs; [
    _1password-gui
    discord
    unstable.slack
    thunderbird
    zoom-us
  ];
}
