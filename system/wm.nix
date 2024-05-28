{ pkgs, ... }:

{
  programs.dconf.enable = true;

  services = {
    dbus = {
      enable = true;
      packages = with pkgs; [
        dconf
      ];
    };

    gnome.gnome-keyring.enable = true;

    xserver = {
      enable = true;
      layout = "us,fr";
      xkbVariant = ",bepo";

      displayManager = {
        defaultSession = "none+xmonad";

        lightdm = {
          enable = true;
          background = ../resources/wallpaper.png;
          greeters.gtk = {
            enable = true;
            clock-format = "%d/%m/%Y %R";
            indicators = [
              "~spacer"
              "~clock"
              "~spacer"
              "~power"
            ];
            theme = {
              package = pkgs.arc-theme;
              name = "Arc-Dark";
            };
            cursorTheme = {
              package = pkgs.capitaine-cursors;
              name = "capitaine-cursors";
            };
            iconTheme = {
              package = pkgs.tela-circle-icon-theme;
              name = "Tela-circle";
            };
          };
        };
      };
      libinput.enable = true;

      windowManager.xmonad.enable = true;
    };
  };
}
