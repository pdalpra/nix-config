name: { nixpkgs, system, revision }:

let baseConfig =
  { config, ... }: {
    config.system.configurationRevision = revision;
    config.networking.hostName = name;
    config.nix.registry.nixpkgs.flake = nixpkgs;
  };
in
nixpkgs.lib.nixosSystem rec {
  inherit system;

  modules = [
    baseConfig
    ../system/machines/${name}.nix
    ../system/configuration.nix
  ];
}