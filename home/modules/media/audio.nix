{ pkgs, config, ... }:

let
  # TODO: move it wav-converter repo as a flake
  wavConverter = with pkgs; rustPlatform.buildRustPackage rec {
    pname = "wav-converter";
    version = "0.0.1";
    cargoHash = "sha256-xKzR1HlbPoNojCBE2tnBu1RdR30rym9p/C2C2Ytnk5Y=ff";
    buildInputs = [ ffmpeg ];
    src = fetchFromGitHub {
      owner = "pdalpra";
      repo = pname;
      rev = "0a724a7e43759eb7f4cbbcf4d7713dc840e6a98d";
      hash = "sha256-anqUdSZQUodOE8+FBPKn1+bTGs49WoSRoY8JBrrj+s0=";
    };
  };
in
{
  home.packages = with pkgs; [
    android-file-transfer
    asunder
    pavucontrol
    quodlibet
    wavConverter
  ];

  # Manage asunder configuration
  xdg.configFile."asunder/asunder" = {
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

  services = {
    easyeffects.enable = true;
    pasystray.enable = true;
  };
}
