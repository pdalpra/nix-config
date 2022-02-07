{ pkgs, ...}:
{
  environment.systemPackages = [ pkgs.cachix ];
  nix = {
    binaryCaches = [
      "https://pdalpra.cachix.org"
    ];
    binaryCachePublicKeys = [
      "pdalpra.cachix.org-1:AsXJMGyrER6tcCaNVD+ENoSFwdc5SYNTlUiHAcdPCH4="
    ];
  };
}