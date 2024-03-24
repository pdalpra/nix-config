{ lib
, overlays
, darwin
, revision
}: system: name:

let
  pkgs = overlays system;
in
darwin.lib.darwinSystem {
  inherit pkgs;

  # TODO : share nix config
  modules = [{
    system.configurationRevision = revision;
    environment.systemPackages = with pkgs; [
      curl
      git
      htop
      ncdu
      tree
      wget
      nix-du
    ];
    networking.dns = [
      "1.1.1.1"
      "9.9.9.9"
      "4.4.4.4"
      "8.8.8.8"
    ];

    homebrew = {
      enable = true;
    };

    programs = {
      zsh.enable = true;
      nix-index.enable = true;
      gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
    };

    system.defaults = {
      ActivityMonitor = {
        ShowCategory = 101; # All Processes, Hierarchally
        SortColumn = "CPUUsage";
        SortDirection = 0;
      };
      dock = {
        autohide = true;
        appswitcher-all-displays = true;
      };
      finder = {
        ShowPathbar = true;
        FXPreferredViewStyle = "icnv"; # icon view
      };
      menuExtraClock = {
        ShowDate = 0;
        Show24Hour = true;
      };
      NSGlobalDomain = {
        "com.apple.mouse.tapBehavior" = 1;
        AppleInterfaceStyle = "Dark";
        NSDocumentSaveNewDocumentsToCloud = false;
        trackpad = {
          Clicking = true;
          TrackpadRightClick = true;
        };
      };
    };

    time.timeZone = "America/New_York";


    nix.useDaemon = true;
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes auto-allocate-uids
      '';
      gc = {
        automatic = true;
        # TODO configure
        #dates = "weekly";
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
  }];
}
