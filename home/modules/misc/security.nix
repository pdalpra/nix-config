{ pkgs, ... }:

{

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
