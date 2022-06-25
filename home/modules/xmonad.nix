{ pkgs, ... }:

{
  programs.alacritty.enable = true;
  services.picom.enable = true;
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: (import ./xmonad-config/packages.nix { inherit haskellPackages; });
    config = ./xmonad-config/xmonad.hs;
  };

}
