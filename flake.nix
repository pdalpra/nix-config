{
  description = "My NixOS and home-manager configurations";
  nixConfig.bash-prompt = "\[nix-config-dev\]$ ";

  inputs = {
    # Package sources
    nixpkgs.url = "nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nurpkgs.url = "github:nix-community/NUR";

    # Additional tools
    agenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Flake libraries
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , nurpkgs
    , agenix
    , home-manager
    , disko
    , flake-utils
    , ...
    }:
    let
      mkISO = import ./lib/mk-iso.nix;
      mkNixOS = import ./lib/mk-nixos.nix;
      mkHM = import ./lib/mk-hm.nix;
      mkDevShell = import ./lib/dev-shell.nix;
      inherit (nixpkgs) lib;
      overlays = import ./lib/overlays.nix { inherit nixpkgs nixpkgs-unstable nurpkgs; };
      system = "x86_64-linux";
      revision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      forAllSystems = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = overlays system;
          agenixBin = agenix.packages.${system}.default;
        in
        {
          formatter = pkgs.nixpkgs-fmt;
          devShell = mkDevShell { inherit pkgs agenixBin; };
        });
    in
    nixpkgs.lib.recursiveUpdate forAllSystems {
      packages.${system}.disko = disko.packages.${system}.default;
      nixosConfigurations = {
        iso = mkISO { inherit nixpkgs system; };
        vm = mkNixOS "vm" { inherit overlays lib home-manager disko system revision; };
      };
      homeConfigurations = {
        pdalpra = mkHM "pdalpra" { inherit overlays home-manager system; };
      };
    };
}
