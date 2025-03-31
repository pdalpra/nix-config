{ pkgs, config, lib, myLib, ... }:

let
  libPath = ./xmonad-config/lib;
  libFiles = lib.pipe libPath [
    myLib.listFiles
    (map (file: { "${file}" = libPath + "/${file}"; }))
    myLib.mergeAll
  ];
  thunar = with pkgs; xfce.thunar.override {
    thunarPlugins = [
      xfce.thunar-volman
      xfce.thunar-archive-plugin
    ];
  };
in
{
  catppuccin.cursors.enable = true;

  home = {
    # Create folder for screenshots
    file."${config.xdg.userDirs.pictures}/screenshots/.keep".text = "";
    pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
    };
  };

  home.packages = with pkgs; [
    feh
    unstable.scrot
    thunar
    xarchiver
    xorg.xbacklight
    xorg.xev
    xorg.xmessage
    xorg.xkill
    xkblayout-state
  ];

  services = {
    picom = {
      enable = true;
      fade = true;
      inactiveOpacity = 1;
    };
    udiskie.enable = true;
  };

  xsession = {
    enable = true;

    initExtra = ''
      1password --silent &
      protonmail-bridge -n &
      brave &
      brave --incognito &
      thunderbird &
      yubikey-touch-detector -libnotify &
      steam -silent &
      blueman-applet &
      feh --bg-scale ${../../../resources/wallpaper.png}
    '';

    windowManager.xmonad = {
      inherit libFiles;
      inherit (pkgs.unstable) haskellPackages;

      enable = true;
      config = ./xmonad-config/config.hs;
      extraPackages = hp: with hp; [
        xmonad-contrib_0_18_1
        dbus
      ];
    };
  };
}
