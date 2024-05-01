{ pkgs, config, lib, myLib, ... }:

with lib;
with myLib;

let
  libPath = ./xmonad-config/lib;
  libFiles = pipe libPath [
    listFiles
    (map (file: { "${file}" = libPath + "/${file}"; }))
    mergeAll
  ];
in
{
  home = {
    # Create folder for screenshots
    file."${config.xdg.userDirs.pictures}/screenshots/.keep".text = "";
  };

  home.packages = with pkgs; [
    feh
    unstable.scrot
    xfce.thunar
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
      chromium &
      chromium --incognito &
      slack &
      thunderbird &
      yubikey-touch-detector -libnotify &
      whatsapp-for-linux &
      feh --bg-scale ${../../../resources/wallpaper.png}
    '';

    windowManager.xmonad = {
      inherit libFiles;

      enable = true;
      config = ./xmonad-config/config.hs;
      extraPackages = hp: with hp; [
        xmonad-contrib
        dbus
      ];
    };
  };
}
