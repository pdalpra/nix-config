{ pkgs, ... }:

{
  home.packages = with pkgs; [
    clang
    rustup
  ];
}
