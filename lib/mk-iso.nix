{ nixpkgs, system }:

let
  baseConfig = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      git
      git-secret
    ];

    nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  };
in
nixpkgs.lib.nixosSystem rec {
  inherit system;

  modules = [
    baseConfig
    ../system/cachix.nix
    "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
  ];
}
