{ pkgs, hm-pkgs, ... }:

{
  users = {
    defaultUserShell = pkgs.bash;
    users = {
      pdalpra = {
        isNormalUser = true;
        uid = 1000;
        home = "/home/pdalpra";
        createHome = true;
        shell = pkgs.zsh;
        packages = [ hm-pkgs.home-manager ];
        extraGroups = [
          "docker"
          "wheel"
        ];
        openssh.authorizedKeys.keys = [
          "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHxXCVW8ElfZeVZ7l7VQI+wmwbtiAO9dzhCEuiNeuiHLLuoFDKes4gbsbgf/fEWkKFPuUyrwlN//k2dAycGtlgo="
        ];
      };
    };
  };
}
