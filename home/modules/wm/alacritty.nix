_:

let
  # Taken from https://github.com/aarowill/base16-alacritty/blob/master/colors/base16-tomorrow-night-eighties.yml
  tomorrowNightEightiesTheme = {
    # Default colors
    primary = {
      background = "0x2d2d2d";
      foreground = "0xcccccc";
    };
    cursor = {
      text = "0x2d2d2d";
      cursor = "0xcccccc";
    };
    # Normal colors
    normal = {
      black = "0x2d2d2d";
      red = "0xf2777a";
      green = "0x99cc99";
      yellow = "0xffcc66";
      blue = "0x6699cc";
      magenta = "0xcc99cc";
      cyan = "0x66cccc";
      white = "0xcccccc";

      # Bright colors
      bright = {
        black = "0x999999";
        red = "0xf99157";
        green = "0x393939";
        yellow = "0x515151";
        blue = "0xb4b7b4";
        magenta = "0xe0e0e0";
        cyan = "0xa3685a";
        white = "0xffffff";
      };
    };
  };
in
{
  programs.alacritty = {
    enable = true;
    settings = {
      decorations = "none";
      font = {
        normal = {
          family = "Berkeley Mono";
          style = "Regular";
        };
        size = 6.0;
        builtin_box_drawing = false;
      };
      colors = tomorrowNightEightiesTheme;
      cursor.thickness = 0.05;
    };
  };
}
