{ pkgs, ... }:

{
  home.packages = with pkgs; [
    libnotify
  ];


  services.dunst = {
    enable = true;
    catppuccin.enable = true;

    iconTheme = {
      package = pkgs.tela-circle-icon-theme;
      name = "Tela-circle";
      size = "16x16";
    };

    settings = {
      global = {
        follow = "mouse";
        width = 400;
        height = 200;
        offset = "30x50";
        transparency = 16;
        text_icon_padding = 5;
        frame_width = 2;
        gap_size = 3;
        font = "Berkeley Mono 15";
        line_height = 3;
      };
    };
  };
}

