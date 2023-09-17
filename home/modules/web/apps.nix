{ pkgs, ... }:

{
  home.packages = with pkgs; [
    _1password-gui
    discord
    protonvpn-gui
    protonvpn-cli
    unstable.protonmail-bridge
    unstable.slack
    thunderbird
    zoom-us
  ];
}
