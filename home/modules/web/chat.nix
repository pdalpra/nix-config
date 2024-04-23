{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    whatsapp-for-linux
    unstable.slack
    zoom-us
  ];
}
