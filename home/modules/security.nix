{ pkgs, ... }:

{

  home.packages = with pkgs;
    [
      yubikey-manager
      yubikey-manager-qt
      yubikey-touch-detector # TODO : setup
    ];
  services.gnome-keyring.enable = true;
}
