{ pkgs, hmPkgs, ... }:

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
        packages = [ hmPkgs.home-manager ];
        extraGroups = [
          "docker"
          "wheel"
        ];
        openssh.authorizedKeys.keys = (import ../keys/pdalpra.nix).sshKeys;
      };
    };
  };
}
