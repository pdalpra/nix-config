{
  description = "My NixOS and home-manager configurations";
  nixConfig.bash-prompt = "\[nix-config-dev\]$ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      mkNixOS  = import ./lib/mk-nixos.nix;
      system   = "x86_64-linux";
      revision =  nixpkgs.lib.mkIf (self ? rev) self.rev;
      allDevShells = flake-utils.lib.eachDefaultSystem(system: {
        devShell = (import ./dev-shell.nix { inherit system nixpkgs; });
      });
    in nixpkgs.lib.recursiveUpdate allDevShells {
      nixosConfigurations = {
        vm = mkNixOS "vm" { inherit nixpkgs system revision; };
      };
    };
}