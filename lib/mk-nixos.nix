name: { nixpkgs, home-manager, system, revision }:

let
  specialArgs = {
    hm-pkgs = home-manager.packages.${system};
  };
  baseConfig = { config, ... }: {
    config.system.configurationRevision = revision;
    config.networking.hostName = name;
    config.nix.registry.nixpkgs.flake = nixpkgs;
  };
in
nixpkgs.lib.nixosSystem rec {
  inherit system specialArgs;

  modules = [
    baseConfig
    ../system/machines/${name}.nix
    ../system/configuration.nix
  ];
}
