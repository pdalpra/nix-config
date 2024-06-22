{ lib
, myLib
, overlays
, home-manager
, agenix
, disko
, impermanence
, nixos-hardware
, catppuccin
, system
, revision
}: name:

let
  pkgs = overlays system;
  hardware = nixos-hardware.nixosModules;
  specialArgs = {
    inherit myLib agenix hardware;
  };
  baseConfig = _: {
    # FIXME : agenix/impermanence mixup
    age.identityPaths = [ "/persistent-system/key" ];
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
    impermanence.nixosModules.impermanence
    catppuccin.nixosModules.catppuccin
    ../modules/common/profile.nix
    ../modules/nixos/impermanence.nix
    ./cachix.nix
    ../system/configuration.nix
    specificConfig
    diskoConfig
    home-manager.nixosModules.home-manager
  ];
}
