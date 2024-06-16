{ lib
, myLib
, overlays
, home-manager
, agenix
, catppuccin
, revision
}: name: profile: system:

let
  pkgs = overlays system;
  specialArgs = {
    inherit myLib agenix profile;
  };
  baseConfig = _: {
    age.identityPaths = [ "/etc/nix/key" ];
    services.nix-daemon.enable = true;
    nix.package = pkgs.nix;
    programs.zsh.enable = true;
    nix.extraOptions = ''
      experimental-features = nix-command flakes no-url-literals repl-flake
    '';
    users.users."x" = {
      shell = pkgs.zsh;
      home = "/Users/x";
    };
    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
      backupFileExtension = "bkp";
      extraSpecialArgs = { inherit myLib; };
      users."x" = {
        imports = [
          agenix.homeManagerModules.default
          ../home/home.nix
        ];
      };
    };

  };
  machineRoot = ../system/machines + "/${name}";
  specificConfig = machineRoot + /configuration.nix;
in
lib.darwinSystem {
  inherit pkgs specialArgs;

  modules = [
    baseConfig
    agenix.nixosModules.default
    home-manager.darwinModules.home-manager
    specificConfig
  ];
}
