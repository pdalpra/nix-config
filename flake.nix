{
  description = "My NixOS and home-manager configurations";
  nixConfig.bash-prompt = "\[nix-config-dev\]$ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, flake-utils }:
    let
      mkNixOS = import ./lib/mk-nixos.nix;
      mkHM = import ./lib/mk-hm.nix;
      system = "x86_64-linux";
      revision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      perSystem = flake-utils.lib.eachDefaultSystem (system: {
        formatter = nixpkgs.legacyPackages.${system}.nixpkgs-fmt;
        devShell = (import ./dev-shell.nix { inherit system nixpkgs; });
      });
    in
    nixpkgs.lib.recursiveUpdate perSystem {
      nixosConfigurations = {
        vm = mkNixOS "vm" { inherit nixpkgs home-manager system revision; };
      };
      homeConfigurations = {
        pdalpra = mkHM "pdalpra" { inherit nixpkgs nixpkgs-unstable home-manager system; };
      };
    };
}
