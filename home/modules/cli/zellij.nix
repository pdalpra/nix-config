{ pkgs, ... }:

{
  catppuccin.zellij.enable = true;

  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
    attachExistingSession = true;
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
      show_startup_tips = false;
    };
  };

  xdg.configFile."zellij/layouts/default.kdl".text = ''
    layout {
      default_tab_template {
        children
        pane size=1 borderless=true {
          plugin location="file:${pkgs.zjstatus}/bin/zjstatus.wasm" {
            format_left   "{mode} #[fg=#89B4FA,bold]{session}"
            format_center "{tabs}"
            format_right  "{datetime}"
            format_space  ""

            hide_frame_for_single_pane "true"

            mode_normal  "#[bg=blue] "
            mode_locked  "#[bg=grey] "

            tab_normal   "#[fg=#6C7086] {name} "
            tab_active   "#[fg=#9399B2,bold,italic] {name} "

            datetime          "#[fg=#6C7086,bold] {format} "
            datetime_format   "%Y-%m-%d %H:%M"
            datetime_timezone "Europe/Paris"
          }
        }
        pane size=2 borderless=true {
          plugin location="zellij:status-bar"
        }
      }
    }
  '';
}
