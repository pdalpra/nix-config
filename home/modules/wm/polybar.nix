{ pkgs, config, lib, ... }:

let
  colorWarning = "#f79836";
  colorPowerMenu = "ef1735";
  polybarLogs = "${config.xdg.dataHome}/polybar/logs";
  systemctl = "${pkgs.systemd}/bin/systemctl";
  loginctl = "${pkgs.systemd}/bin/loginctl";
  common = {
    font-0 = "Berkeley Mono:size=12:style=Regular";
    font-1 = "Font Awesome 6 Free,Font Awesome 6 Free Solid:size=12:style=Solid";
    font-2 = "Font Awesome 6 Free,Font Awesome 6 Free Regular:size=12:style=Regular";

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
        "$DRY_RUN_CMD ${systemctl} --user restart polybar.service || true";
    };

    packages = with pkgs; [
      font-awesome
    ];
  };

  services.polybar = {
    enable = true;
    catppuccin.enable = true;

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
        modules-left = "cpu memory temperature disk";
        modules-center = "tray";
        modules-right = "keyboard sound backlight";
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
      "module/powermenu" = {
        type = "custom/menu";
        expand-right = false;
        format-spacing = 1;

        label-open = "";
        label-open-foreground = colorPowerMenu;
        label-close = "";
        label-close-foreground = colorPowerMenu;
        label-separator = "|";

        menu-0-0 = "Lock";
        menu-0-0-exec = "${loginctl} lock-session";
        menu-0-0-foreground = colorPowerMenu;
        menu-0-1 = "Logout";
        menu-0-1-exec = "${loginctl} kill-session self";
        menu-0-1-foreground = colorPowerMenu;
        menu-0-2 = "Suspend";
        menu-0-2-exec = "${systemctl} suspend";
        menu-0-2-foreground = colorPowerMenu;
        menu-0-3 = "Reboot";
        menu-0-3-exec = "${systemctl} reboot";
        menu-0-3-foreground = colorPowerMenu;
        menu-0-4 = "Poweroff";
        menu-0-4-exec = "${systemctl} poweroff";
        menu-0-4-foreground = colorPowerMenu;
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

      ###################
      # BOTTOM - CENTER #
      ###################

      "module/tray" = {
        type = "internal/tray";
        tray-size = "90%";
        tray-padding = "5px";
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
        format = "%{T2} %{T-} <label>";
        enable-scroll = true;
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
