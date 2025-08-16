{ pkgs, ... }:

{
  home.packages = with pkgs; [
    protonvpn-gui
    electron-mail
  ];
}
