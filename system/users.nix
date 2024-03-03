{ pkgs, hmPkgs, config, ... }:

{
  # Secrets
  age.secrets.pdalpra.file = ../secrets/pdalpra.age;
  age.secrets.root.file = ../secrets/root.age;

  users = {
    mutableUsers = false;
    defaultUserShell = pkgs.bash;

    users = {
      root.hashedPasswordFile = config.age.secrets.root.path;
      pdalpra = {
        isNormalUser = true;
        uid = 1000;
        hashedPasswordFile = config.age.secrets.pdalpra.path;
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
