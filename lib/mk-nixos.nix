name: { overlays, lib, home-manager, disko, agenix, system, revision }:

let
  pkgs = overlays system;
  specialArgs = {
    hmPkgs = home-manager.packages.${system};
    myLib = import ./utils.nix { inherit (pkgs) lib; };
  };
  baseConfig = _: {
    age.identityPaths = [ "/etc/agenix/key" ];
    system.configurationRevision = revision;
    networking.hostName = name;
  };
  machineRoot = ../system/machines + "/${name}";
  specificConfig = machineRoot + /configuration.nix;
  diskoConfig = import (machineRoot + /disks.nix) { };
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
  ];
}
