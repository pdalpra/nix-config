{ lib, pkgs, config, ... }:

lib.mkIf (!builtins.elem "headless" config.profile) {
  home.packages = [
    pkgs.unstable.qmk
    pkgs.qmk_hid
    pkgs.unstable.bazecor
  ];
}
