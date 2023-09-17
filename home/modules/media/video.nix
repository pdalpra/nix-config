{ pkgs, ... }:

{
  home.packages = with pkgs; [
    mupdf
    vlc
    image-roll
  ];
}
