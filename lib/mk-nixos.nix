{ lib
, myLib
, overlays
, home-manager
, agenix
, disko
, impermanence
, system
, revision
}: name:

let
  pkgs = overlays system;
  persistence = {
    system = "/persistent-system";
    homes = "/persistent-homes";
  };
  specialArgs = {
    inherit myLib agenix impermanence persistence;
  };
  baseConfig = _: {
    age.identityPaths = [ "${persistence.system}/key" ];
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
    impermanence.nixosModules.impermanence
    home-manager.nixosModules.home-manager
  ];
}
