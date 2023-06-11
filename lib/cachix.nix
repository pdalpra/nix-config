{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.cachix ];
  nix.settings = {
    substituters = [
      "https://pdalpra.cachix.org"
    ];
    trusted-public-keys = [
      "pdalpra.cachix.org-1:AsXJMGyrER6tcCaNVD+ENoSFwdc5SYNTlUiHAcdPCH4="
    ];
  };
}
