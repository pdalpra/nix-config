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
    impermanence.url = "github:nix-community/impermanence";

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
    , flake-utils
    , ...
    }:
    let
      inherit (nixpkgs) lib;
      myLib = import ./lib/utils.nix { inherit lib; };
      overlays = import ./lib/overlays.nix { inherit nixpkgs nixpkgs-unstable nurpkgs; };
      system = "x86_64-linux";
      revision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      mkISO = import ./lib/mk-iso.nix;
      mkNixOS = import ./lib/mk-nixos.nix {
        inherit lib myLib overlays home-manager agenix disko impermanence system revision;
      };
      mkHM = import ./lib/mk-hm.nix { inherit myLib overlays agenix home-manager system; };
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
            checks.lint = import ./lib/lint.nix { inherit pkgs lintingPkgs; };
            devShells.default = import ./lib/dev-shell.nix { inherit pkgs lintingPkgs agenixBin; };
          });
    in
    nixpkgs.lib.recursiveUpdate forAllSystems {
      packages.${system}.disko = disko.packages.${system}.default;
      nixosConfigurations = {
        iso = mkISO { inherit nixpkgs system; };
        vm = mkNixOS "vm";
      };
      homeConfigurations = {
        pdalpra = mkHM "pdalpra";
      };
    };
}
