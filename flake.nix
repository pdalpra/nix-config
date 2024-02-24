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
      system = "x86_64-linux";
      revision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      perSystem = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          unstable = nixpkgs-unstable.legacyPackages.${system};
          agenixBin = agenix.packages.${system}.default;
        in
        {
          formatter = pkgs.nixpkgs-fmt;
          devShell = import ./lib/dev-shell.nix { inherit pkgs unstable agenixBin; };
        });
    in
    nixpkgs.lib.recursiveUpdate perSystem {
      nixosConfigurations = {
        iso = mkISO { inherit nixpkgs system; };
        vm = mkNixOS "vm" { inherit nixpkgs home-manager disko system revision; };
      };
      homeConfigurations = {
        pdalpra = mkHM "pdalpra" { inherit nixpkgs nixpkgs-unstable nurpkgs home-manager system; };
      };
    };
}
