{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    android-file-transfer
    asunder
    quodlibet
  ];

  # Manage asunder configuration
  home.file."${config.xdg.configHome}/asunder/asunder" = {
    text = ''
      /dev/cdrom
      ${config.home.homeDirectory}/CD Rips
      0
      1
      %N %T
      %A - %L
      %A/%L
      1
      0
      0
      0
      1
      10
      6
      5
      unused
      1436
      596
      1
      1
      0
      10.0.0.1
      8080
      0
      1
      1
      3
      0
      gnudb.gnudb.org
      8880
      0
      2
      unused
      unused
      0
      2
      0
      0
      0
      9
      0
      0
      0
      2
      0
      10
      /
    '';
  };

  services.pasystray.enable = true;
}
