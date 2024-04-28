{ pkgs, ... }: {

  home.packages = [
    pkgs.unstable.qmk
    pkgs.qmk_hid
    pkgs.unstable.bazecor
  ];
}
