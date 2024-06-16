{ pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    _1password-gui
    yubikey-manager
    #yubikey-manager-qt
    #yubikey-touch-detector
  ];

  services = lib.mkIf pkgs.stdenv.isLinux {
    ssh-agent.enable = true;
  };
}
