{ pkgs, config, myLib, agenix, catppuccin, ... }:

let
  networkManager = myLib.mkIfElse
    config.networking.networkmanager.enable
    [ "networkmanager" ]
    [ ];
  optionalGroups = networkManager;
in
{

  # Secrets
  age.secrets = {
    pdalpra.file = ../secrets/pdalpra.age;
    root.file = ../secrets/root.age;
  };

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
          "audio"
          "video"
        ] ++ optionalGroups;
        openssh.authorizedKeys.keys = (import ../keys/pdalpra.nix).sshKeys;
      };
    };
  };
  home-manager = {
    backupFileExtension = "bkp";
    extraSpecialArgs = { inherit myLib; };
    useUserPackages = true;
    useGlobalPkgs = true;

    users.pdalpra = {
      imports = [
        ../modules/common/profile.nix
        agenix.homeManagerModules.default
        catppuccin.homeManagerModules.catppuccin
        ../home/home.nix
      ];

      inherit (config) profile;
      catppuccin = {
        inherit (config.catppuccin) flavor accent;
      };
    };
  };
}
