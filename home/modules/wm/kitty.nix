_:

{
  programs.kitty = {
    enable = true;
    font = {
      name = "Berkeley Mono";
      size = 14;
    };
    theme = "Tomorrow Night Eighties";

    settings = {
      enable_audio_bell = false;
      scrollback_lines = 2000;
      tab_bar_style = "powerline";
      tab_powerline_style = "slanted";
    };
  };
}
