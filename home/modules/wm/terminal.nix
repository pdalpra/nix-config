_:

{
  programs.kitty = {
    enable = true;
    extraConfig = ''
      font_family Berkeley Mono
      font_size 14
      strip_trailing_spaces auto
      enable_audio_bell no
      hide_window_decorations yes
    '';
  };
}
