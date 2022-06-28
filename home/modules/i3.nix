{ pkgs, ... }:

{
  services.picom.enable = true;

  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      config = {
        terminal = "alacritty";
        gaps = {
          smartGaps = true;
          inner = 2;
          #    outer = 0;
        };
      };
    };
  };

}
