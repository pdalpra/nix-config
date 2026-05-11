{
  description = "My NixOS configurations";
  nixConfig.bash-prompt = "\[nix-config-dev\]$ ";

  inputs = {
    # Package sources
    nixpkgs.url = "nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nurpkgs.url = "github:nix-community/NUR";

    # Additional tools
    agenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    catppuccin.url = "github:catppuccin/nix";
    zjstatus = {
      url = "github:dj95/zjstatus";
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
    , impermanence
    , nixos-hardware
    , catppuccin
    , flake-utils
    , zjstatus
    , ...
    }:
    let
      inherit (nixpkgs) lib;
      myLib = import ./lib/utils.nix {
        inherit lib;
      };
      overlays = import ./lib/overlays.nix {
        inherit nixpkgs nixpkgs-unstable nurpkgs zjstatus;
      };
      system = "x86_64-linux";
      revision = lib.mkIf (self ? rev) self.rev;
      mkISO = import ./lib/mk-iso.nix {
        inherit nixpkgs system;
      };
      mkNixOS = import ./lib/mk-nixos.nix {
        inherit
          lib
          myLib
          overlays
          home-manager
          agenix
          disko
          impermanence
          nixos-hardware
          catppuccin
          system
          revision;
      };
      mkHome = import ./lib/mk-home.nix {
        inherit
          myLib
          overlays
          home-manager
          agenix
          catppuccin;
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
      packages.${system}.disko = disko.packages.${system}.default;
      nixosConfigurations = {
        iso = mkISO;
        vm = mkNixOS "vm" [ ];
        fw16 = mkNixOS "fw16" [ ];
      };
      homeConfigurations = {
        "headless-x86_64" = mkHome "pdalpra" [ "headless" ] "x86_64-linux" [ ];
        "headless-aarch64" = mkHome "pdalpra" [ "headless" ] "aarch64-linux" [ ];
      };
      lib = {
        inherit mkHome mkNixOS;
      };
    };
}
