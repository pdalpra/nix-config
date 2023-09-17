{ pkgs, config, ... }:

let
  rofiThemes = pkgs.fetchFromGitHub {
    owner = "lr-tech";
    repo = "rofi-themes-collection";
    rev = "5ae9b23ef58893229b0df57ad750ad84801a632e";
    sha256 = "ecCQcDVWXpSilER99OROW9wutIq58llUGjFTn9rH2RM=";
  };
  rofiTheme = theme: rofiThemes + "/themes/${theme}.rasi";
in
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

    cycle = true;
    font = "Berkeley Mono 20";
    theme = rofiTheme "rounded-nord-dark";

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
