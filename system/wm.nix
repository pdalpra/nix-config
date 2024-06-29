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

    libinput.enable = true;

    displayManager.defaultSession = "none+xmonad";

    gnome.gnome-keyring.enable = true;

    xserver = {
      enable = true;
      xkb = {
        layout = "us,fr";
        variant = ",bepo";
      };

      displayManager.lightdm = {
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
            package = pkgs.catppuccin-cursors.mochaGreen;
            name = "mochaGreen";
          };
          iconTheme = {
            package = pkgs.tela-circle-icon-theme;
            name = "Tela-circle";
          };
        };
      };

      windowManager.xmonad.enable = true;
    };
  };
}
