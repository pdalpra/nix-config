{ pkgs, lib, config, ... }:

lib.mkIf (!builtins.elem "headless" config.profile) {
  home.packages = with pkgs; [
    protonvpn-gui
    electron-mail
  ];
}
