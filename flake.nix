{
  description = "My NixOS configurations";
  nixConfig.bash-prompt = "\[nix-config-dev\]$ ";

  inputs = {
    # Package sources
    nixpkgs.url = "nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nurpkgs.url = "github:nix-community/NUR";

    # Additional tools
    agenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    catppuccin.url = "github:catppuccin/nix";

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
    , nix-darwin
    , disko
    , impermanence
    , nixos-hardware
    , catppuccin
    , flake-utils
    , ...
    }:
    let
      myLib = import ./lib/utils.nix {
        inherit (nixpkgs) lib;
      };
      overlays = import ./lib/overlays.nix {
        inherit nixpkgs nixpkgs-unstable nurpkgs;
      };
      linuxSystem = "x86_64-linux";
      macosSystem = "aarch64-darwin";
      revision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      mkISO = import ./lib/mk-iso.nix {
        inherit nixpkgs;
      };
      mkNixOS = import ./lib/mk-nixos.nix {
        inherit (nixpkgs) lib;

        inherit
          myLib
          overlays
          home-manager
          agenix
          disko
          impermanence
          nixos-hardware
          catppuccin
          revision;
      };
      mkDarwin = import ./lib/mk-darwin.nix {
        inherit (nix-darwin) lib;

        inherit
          myLib
          overlays
          home-manager
          agenix
          catppuccin
          revision;
      };
      forAllSystems = flake-utils.lib.eachDefaultSystem
        (system:
          let
            pkgs = overlays system;
            agenixBin = agenix.packages.${system}.default;
            lintingPkgs = with pkgs; [
              treefmt
              statix
              deadnix
              haskellPackages.cabal-fmt
              haskellPackages.fourmolu
              yamlfix
              nixpkgs-fmt
            ];
          in
          {
            formatter = pkgs.nixpkgs-fmt;
            checks.lint = import ./lib/lint.nix {
              inherit pkgs lintingPkgs;
            };
            devShells.default = import ./lib/dev-shell.nix {
              inherit pkgs lintingPkgs agenixBin;
            };
          });
    in
    nixpkgs.lib.recursiveUpdate forAllSystems {
      packages.${linuxSystem}.disko = disko.packages.${linuxSystem}.default;
      nixosConfigurations = {
        iso = mkISO linuxSystem;
        vm = mkNixOS "vm" "perso" linuxSystem;
      };
      darwinConfigurations = {
        work-mbp = mkDarwin "work-mbp" "work" macosSystem;
      };
    };
}
