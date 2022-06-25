{ pkgs, ... }:

{
  services = {
    gnome.gnome-keyring.enable = true;
    upower.enable = true;

    dbus = {
      enable = true;
    };

    xserver = {
      enable = true;

      windowManager.xmonad.enable = true;

      displayManager.lightdm = {
        enable = true;
      };

      libinput = {
        enable = true;
      };
    };
  };

  systemd.services.upower.enable = true;
}
