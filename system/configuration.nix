{ pkgs, ... }:
{
  imports = [
    ./cachix.nix
    ./apps.nix
    ./users.nix
  ];

  time.timeZone      = "Europe/Paris";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font   = "Lat2-Terminus16";
    keyMap = "fr-bepo";
  };

  documentation.nixos = {
    enable            = true;
    includeAllModules = true;
  };

  networking = {
    firewall.enable = false;
    useDHCP         = false;
    nameservers = [
      "1.1.1.1"
      "9.9.9.9"
      "4.4.4.4"
      "8.8.8.8"
    ];
    timeServers = [
      "0.pool.ntp.org"
      "1.pool.ntp.org"
    ];
  };

  nix = {
    autoOptimiseStore = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 14d";
    };
    trustedUsers = [
      "root"
      "@wheel"
    ];
  };

  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
