{ lib, ... }:
{
  imports = [
    ./apps.nix
    ./networking.nix
    ./users.nix
    ./impermanence.nix
    ./wm.nix
  ];

  boot.loader = {
    efi.canTouchEfiVariables = true;
    grub = {
      enable = true;
      device = "nodev";
      efiSupport = true;
      memtest86.enable = true;
      extraEntries = ''
        menuentry "Reboot" {
          reboot
        }
        menuentry "Poweroff" {
          halt
        }
      '';
    };

  };

  time.timeZone = "Europe/Paris";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = lib.mkDefault "us";
  };

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "green";
  };

  documentation.nixos = {
    enable = true;
    includeAllModules = true;
  };

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes no-url-literals
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
    settings = {
      warn-dirty = false;
      auto-optimise-store = true;
      trusted-users = [
        "root"
        "@wheel"
      ];
    };
  };

  system.stateVersion = "22.11";
}
