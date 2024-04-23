{ pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.protonvpn-gui
    unstable.protonmail-bridge
    thunderbird
  ];
}
