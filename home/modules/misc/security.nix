{ pkgs, ... }:

{

  home.packages = with pkgs;
    [
      _1password-gui
      pass-secret-service
      yubikey-manager
      yubikey-manager-qt
      yubikey-touch-detector # TODO : setup
    ];
  services.gnome-keyring.enable = true;
}
