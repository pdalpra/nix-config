name: { nixpkgs, home-manager, disko, system, revision }:

let
  specialArgs = {
    hm-pkgs = home-manager.packages.${system};
    my-utils = import ./utils.nix { inherit (nixpkgs) lib; };
  };
  baseConfig = _: {
    system.configurationRevision = revision;
    networking.hostName = name;
    nix.registry.nixpkgs.flake = nixpkgs;
  };
  machineRoot = ../system/machines + "/${name}";
in
nixpkgs.lib.nixosSystem {
  inherit system specialArgs;

  modules = [
    baseConfig
    disko.nixosModules.disko
    ./cachix.nix
    ../system/configuration.nix
    (machineRoot + /configuration.nix)
    (machineRoot + /disks.nix)
  ];
}
