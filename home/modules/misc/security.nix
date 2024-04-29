{ pkgs, ... }:

{

  home.packages = with pkgs; [
    _1password-gui
    yubikey-manager
    yubikey-manager-qt
    yubikey-touch-detector
  ];

  services = {
    gnome-keyring.enable = true;
    ssh-agent.enable = true;
  };
}
