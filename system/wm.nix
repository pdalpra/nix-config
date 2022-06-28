{ pkgs, ... }:

{
  services = {
    gnome.gnome-keyring.enable = true;
    upower.enable = true;

    dbus.enable = true;

    xserver = {
      enable = true;

      windowManager.i3.enable = true;
      
      displayManager = {
        defaultSession = "none+i3";
        
        lightdm = {
          enable = true;
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
              name = "Arc";
            };
            cursorTheme = {
              package = pkgs.capitaine-cursors;
              name = "capitaine-cursors";
            };
            iconTheme = {
              package = pkgs.arc-icon-theme;
              name = "Arc";
            };
          };
        };
      };

      libinput = {
        enable = true;
      };
    };
  };

  systemd.services.upower.enable = true;
}
