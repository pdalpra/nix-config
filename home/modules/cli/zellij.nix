{ pkgs, ... }:

{
  catppuccin.zellij.enable = true;

  programs.zellij = {
    enable = true;
    package = pkgs.unstable.zellij;
    settings = {
      default_shell = "zsh";
      pane_frames = false;
      copy_on_select = true;
      mouse_mode = true;
      scroll_buffer_size = 50000;
      on_force_close = "detach";
      session_serialization = true;
      serialize_pane_viewport = true;
    };
  };
}
