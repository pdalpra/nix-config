{ pkgs, ... }:

{
  programs = {
    fzf = {
      enable = true;
      enableZshIntegration = true;

      package = pkgs.unstable.fzf;
      defaultOptions = [
        "--height=60%"
        "--border=rounded"
        "--prompt='>>>  '"
        "--marker='✓ '"
        "--pointer='> '"
        "--preview-window=border-rounded"
        "--separator='='"
        "--scrollbar='|'"
      ];
      colors = {
        fg = "-1";
        "fg+" = "#d0d0d0";
        bg = "-1";
        "bg+" = "#262626";
        hl = "#5f87af";
        "hl+" = "#5fd7ff";
        info = "#afaf87";
        marker = "#87ff00";
        prompt = "#ff002f";
        spinner = "#5e94ff";
        pointer = "#e71c11";
        header = "#87afaf";
        border = "#262626";
        label = "#aeaeae";
        query = "#d9d9d9";
      };
      fileWidgetOptions = [
        "--preview='${pkgs.bat}/bin/bat --color always {}'"
        "--walker-skip=.git,node_modules,target"
      ];
      historyWidgetOptions = [
        "--history-size=2000"
      ];
    };
  };
}
