{
  description = "My NixOS and home-manager configurations";
  nixConfig.bash-prompt = "\[nix-config-dev\]$ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nurpkgs.url = "github:nix-community/NUR";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, nurpkgs, home-manager, flake-utils, ... }:
    let
      mkNixOS = import ./lib/mk-nixos.nix;
      mkHM = import ./lib/mk-hm.nix;
      system = "x86_64-linux";
      revision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      perSystem = flake-utils.lib.eachDefaultSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          formatter = pkgs.nixpkgs-fmt;
          devShell = import ./lib/dev-shell.nix { inherit pkgs; };
        });
    in
    nixpkgs.lib.recursiveUpdate perSystem {
      nixosConfigurations = {
        vm = mkNixOS "vm" { inherit nixpkgs home-manager system revision; };
      };
      homeConfigurations = {
        pdalpra = mkHM "pdalpra" { inherit nixpkgs nixpkgs-unstable nurpkgs home-manager system; };
      };
    };
}
