{ pkgs, ... }:

{
  services.picom = {
    enable = true;
    fade = true;
    inactiveOpacity = "0.8";
  };

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      config = ./xmonad-config/config.hs;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.monad-logger
      ];

    };
  };
}
