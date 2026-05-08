{ pkgs, lib, config, ... }:

lib.mkIf (!builtins.elem "headless" config.profile) {
  home.packages = with pkgs; [
    _1password-gui
    yubikey-manager
    yubioath-flutter
    yubikey-touch-detector
  ];

  services = {
    ssh-agent.enable = true;
  };
}
