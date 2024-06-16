{ nixpkgs }: system:

let
  baseConfig = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      git
    ];

    systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
    users.users.root.openssh.authorizedKeys.keys = (import ../keys/pdalpra.nix).sshKeys;

    nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  };
in
nixpkgs.lib.nixosSystem {
  inherit system;

  modules = [
    baseConfig
    ./cachix.nix
    "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
  ];
}
