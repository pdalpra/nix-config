{ pkgs, ... }:

{
  programs = {
    fzf = {
      enable = true;
      enableZshIntegration = true;
      catppuccin.enable = true;

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
