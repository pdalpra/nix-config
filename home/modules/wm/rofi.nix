{ pkgs, config, ... }:

{
  home.file."${config.xdg.configHome}/rofimoji.rc" = {
    text = ''
      action = copy
      skin-tone = medium-light
    '';
  };

  home.packages = with pkgs; [
    rofimoji
  ];

  programs.rofi = {
    enable = true;
    catppuccin.enable = true;
    cycle = true;
    font = "Berkeley Mono 20";

    extraConfig = {
      modi = builtins.concatStringsSep "," [
        "drun"
        "window"
        "windowcd"
        "filebrowser"
        "pm:${pkgs.rofi-power-menu}/bin/rofi-power-menu"
      ];
      show-icons = true;
      matching = "prefix";
    };
  };
}
