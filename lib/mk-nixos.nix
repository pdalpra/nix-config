name: { nixpkgs, home-manager, system, revision }:

let
  specialArgs = {
    hm-pkgs = home-manager.packages.${system};
  };
  baseConfig = { ... }: {
    system.configurationRevision = revision;
    networking.hostName = name;
    nix.registry.nixpkgs.flake = nixpkgs;
  };
in
nixpkgs.lib.nixosSystem rec {
  inherit system specialArgs;

  modules = [
    baseConfig
    ./cachix.nix
    (../system/machines + "/${name}.nix")
    ../system/configuration.nix
  ];
}
