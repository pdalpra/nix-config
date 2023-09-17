{ pkgs, ... }:

{
  home.packages = with pkgs; [
    libnotify
  ];


  services.dunst = {
    enable = true;

    iconTheme = {
      package = pkgs.tela-circle-icon-theme;
      name = "Tela-circle";
      size = "16x16";
    };

    settings = {
      global = {
        follow = "mouse";
        width = 200;
        height = 200;
        offset = "30x50";
        transparency = 16;
        text_icon_padding = 5;
        frame_width = 2;
        frame_color = "#282a36";
        gap_size = 3;
        font = "Berkeley Mono 15";
        line_height = 3;
      };
      urgency_low = {
        background = "#282a36";
        foreground = "#6272a4";
        timeout = 10;
      };
      urgency_normal = {
        background = "#282a36";
        foreground = "#bd93f9";
        timeout = 10;
      };
      urgency_critical = {
        background = "#ff5555";
        foreground = "#f8f8f2";
        frame_color = "#ff5555";
        timeout = 0;
      };
    };
  };
}

