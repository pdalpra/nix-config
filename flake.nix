{
  description = "My NixOS and home-manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";
  };

  outputs = { self, nixpkgs }:
    let
      mkNixOS  = import ./lib/mk-nixos.nix;
      system   = "x86_64-linux";
      revision =  nixpkgs.lib.mkIf (self ? rev) self.rev;
    in {
      nixosConfigurations = {
        vm =  mkNixOS "vm" { inherit nixpkgs system revision; };
      };
    };
}