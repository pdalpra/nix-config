{ pkgs, config, lib, ... }:

let
  colorWarning = "#f79836";
  polybarLogs = "${config.xdg.dataHome}/polybar/logs";
  systemctl = "${pkgs.systemd}/bin/systemctl";
  common = {
    font-0 = "JetbrainsMono Nerd Font:size=16:style=Medium,Regular";
    font-1 = "Font Awesome 6 Free,Font Awesome 6 Free Solid:size=16:style=Solid";
    font-2 = "Font Awesome 6 Free,Font Awesome 6 Free Regular:size=16:style=Regular";

    enable-ipc = true;
    module-margin = 1;
    padding-right = 1;
  };
in
{
  # Ensures that the polybar logs folder is created
  home = {
    file."${polybarLogs}/.keep".text = "";

    # Restarts polybar automatically
    activation = {
      restartPolybar = lib.hm.dag.entryAfter [ "writeBoundary" ]
        "$DRY_RUN_CMD ${systemctl} --user restart polybar.service";
    };

    packages = with pkgs; [
      font-awesome
    ];
  };

  services.polybar = {
    enable = true;

    package = pkgs.polybar.override {
      iwSupport = true;
      pulseSupport = true;
    };

    script = ''
      polybar top 2> ${polybarLogs}/top.log & disown
      polybar bottom 2> ${polybarLogs}/bottom.log & disown
    '';

    # TODO : add network and bluetooth
    config = {
      "bar/top" = common // {
        modules-left = "workspaces layout";
        modules-center = "title";
        modules-right = "date time battery powermenu";
      };
      "bar/bottom" = common // {
        bottom = true;
        tray-position = "center";
        modules-left = "cpu memory temperature disk";
        modules-right = "keyboard sound backlight";
        tray-maxsize = 16;
        tray-scale = "1.6";
      };

      ##############
      # TOP - LEFT #
      ##############

      "module/workspaces" = {
        type = "internal/xworkspaces";
        format = "  <label-state>";
        label-active-foreground = "#2e9afe";
        label-urgent-foreground = "#ea4300";
        label-empty-foreground = "#7f7f7f";
      };
      "module/layout" = {
        type = "custom/script";
        exec = "${pkgs.xmonad-log}/bin/xmonad-log";
        tail = true;
        interval = 1;
      };

      ################
      # TOP - CENTER #
      ################

      "module/title" = {
        type = "internal/xwindow";
        label-empty = "";
        label-empty-foregroud = colorWarning;
      };

      ###############
      # TOP - RIGHT #
      ###############

      "module/date" = {
        type = "internal/date";
        date = "  %d/%m/%Y";
      };
      "module/time" = {
        type = "internal/date";
        date = "  %H:%M";
      };
      "module/battery" = {
        type = "internal/battery";
        battery = "BAT1";
        adapter = "ACAD";
        time-format = "%H:%M";
        label-charging = "  %percentage%% (%time%)";
        label-discharging = "  %percentage%% (%time%)";
        label-low = "  %percentage%% (%time%)";
        label-low-foreground = colorWarning;
      };
      "module/powermenu" = rec {
        type = "custom/menu";
        expand-right = false;
        format-spacing = 1;

        label-open = "";
        label-open-foreground = "#EF1735";
        label-close = "";
        label-close-foreground = label-open-foreground;
        label-separator = "|";

        menu-0-0 = "Suspend";
        menu-0-0-exec = "${systemctl} suspend";
        menu-0-1 = "Reboot";
        menu-0-1-exec = "${systemctl} reboot";
        menu-0-2 = "Poweroff";
        menu-0-2-exec = "${systemctl} poweroff";
      };

      #################
      # BOTTOM - LEFT #
      #################


      "module/cpu" = rec {
        type = "internal/cpu";
        warn-percentage = 95;
        label = "%{T2}%{T-} %percentage%%";
        label-warn = label;
        label-warn-foreground = colorWarning;
      };
      "module/memory" = {
        type = "internal/memory";
        label = "  %percentage_used%% / %percentage_swap_used%%";
      };
      "module/temperature" = {
        type = "internal/temperature";
        format = " <label>";
      };
      "module/disk" = {
        type = "internal/fs";
        mount-0 = "/";
        format-mounted = "  <label-mounted>";
        label-mounted = "%mountpoint% %percentage_free%%";
      };

      ##################
      # BOTTOM - RIGHT #
      ##################

      "module/sound" = {
        type = "internal/pulseaudio";
        format-volume = "  <label-volume>";
        format-muted = "";
      };
      "module/backlight" = {
        type = "internal/backlight";
        format = "ﯦ <label>";
      };
      "module/keyboard" = {
        type = "internal/xkeyboard";
        blacklist-0 = "num lock";
        blacklist-1 = "scroll lock";
        label-layout = "%{T3}%{T-}  %icon%";
        layout-icon-0 = "us;US (Qwerty)";
        layout-icon-1 = "fr;bepo;FR (Bépo)";
      };
    };
  };
}
