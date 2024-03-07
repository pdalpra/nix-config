name: { lib
      , myLib
      , overlays
      , home-manager
      , agenix
      , disko
      , system
      , revision
      }:

let
  pkgs = overlays system;
  specialArgs = { inherit myLib; };
  baseConfig = _: {
    age.identityPaths = [ "/etc/agenix/key" ];
    system.configurationRevision = revision;
    networking.hostName = name;
  };
  machineRoot = ../system/machines + "/${name}";
  specificConfig = machineRoot + /configuration.nix;
  diskoConfig = machineRoot + /disks.nix;
in
lib.nixosSystem {
  inherit system specialArgs pkgs;

  modules = [
    baseConfig
    agenix.nixosModules.default
    disko.nixosModules.disko
    ./cachix.nix
    ../system/configuration.nix
    specificConfig
    diskoConfig
    home-manager.nixosModules.home-manager
  ];
}
