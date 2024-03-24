_:
{
  imports = [
    ./apps.nix
    ./users.nix
    ./impermanence.nix
    ./wm.nix
  ];

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      memtest86.enable = true;
    };
  };

  time.timeZone = "Europe/Paris";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "fr-bepo";
  };

  documentation.nixos = {
    enable = true;
    includeAllModules = true;
  };

  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = true;
  };

  networking = {
    firewall.enable = false;
    useDHCP = true;
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
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
    settings = {
      auto-optimise-store = true;
      trusted-users = [
        "root"
        "@wheel"
      ];
    };
  };

  system.stateVersion = "22.11";
}
