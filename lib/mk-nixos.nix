{ overlays
, home-manager
, agenix
, disko
, impermanence
, system
, revision
, nixosSystem
}: name:

let
  pkgs = overlays system;
  inherit (pkgs) lib;
  specialArgs = { inherit agenix impermanence; };
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
nixosSystem {

  inherit lib system specialArgs pkgs;

  modules = [
    baseConfig
    agenix.nixosModules.default
    disko.nixosModules.disko
    impermanence.nixosModules.impermanence
    ../modules/nixos/impermanence.nix
    ./cachix.nix
    ../system/configuration.nix
    specificConfig
    diskoConfig
    home-manager.nixosModules.home-manager
  ];
}
