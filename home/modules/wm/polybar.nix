{ pkgs, config, ... }:

let
  polybarLogs = "${config.xdg.configHome}/polybar/logs";
  font = "JetBrainsMono Nerd Font:size=14:style=Medium,Regular";
in
{
  # Ensures that the polybar logs folder is created
  home.file."${polybarLogs}/.keep".text = "";

  services.polybar = {
    enable = true;

    script = ''

      polybar top 2> ${polybarLogs}/top.log & disown
      polybar bottom 2> ${polybarLogs}/bottom.log & disown
    '';

    config = {
      "bar/top" = {
        font-0 = font;
        modules-left = "xmonad";
        modules-center = "window-title";
        tray-position = "right";
        tray-maxsize = 32;
        tray-detached = true;
      };
      "bar/bottom" = {
        font-0 = font;
        bottom = true;
        modules-right = "date-time";
      };

      "module/cpu" = {
        type = "internal/cpu";
      };
      "module/date-time" = {
        type = "internal/date";
        date = "%d/%m/%Y %H:%M";
      };
      "module/window-title" = {
        type = "internal/xwindow";
      };
      "module/xmonad" = {
        type = "custom/script";
        exec = "${pkgs.xmonad-log}/bin/xmonad-log";
        tail = true;
        interval = 1;
      };
    };
  };
}
