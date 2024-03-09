{ pkgs, config, myLib, ... }:

{

  # Secrets
  age.secrets.pdalpra.file = ../secrets/pdalpra.age;
  age.secrets.root.file = ../secrets/root.age;

  programs.fuse.userAllowOther = true;

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
        extraGroups = [
          "docker"
          "wheel"
        ];
        openssh.authorizedKeys.keys = (import ../keys/pdalpra.nix).sshKeys;
      };
    };
  };
  home-manager = {
    backupFileExtension = "bkp";
    extraSpecialArgs = { inherit myLib; };
    useUserPackages = true;
    useGlobalPkgs = true;

    users.pdalpra = ../home/home.nix;
  };
}
