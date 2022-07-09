{ pkgs, lib, ... }:

with lib;
with builtins;

let
  libPath = ./xmonad-config/lib;
  libFiles = pipe libPath [
    readDir
    attrNames
    (map (file: { "${file}" = toPath "${libPath}/${file}"; }))
    (foldl mergeAttrs { })
  ];
in
{
  home.packages = with pkgs; [
    feh
    xorg.xev
    xorg.xmodmap
  ];

  services.picom = {
    enable = true;
    fade = true;
    inactiveOpacity = "0.8";
  };

  xsession = {
    enable = true;

    initExtra = ''
      feh --bg-scale ${../../../resources/wallpaper.png}
    '';

    windowManager.xmonad = {
      inherit libFiles;

      enable = true;
      config = ./xmonad-config/config.hs;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.dbus
      ];
    };
  };
}
