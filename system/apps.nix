{ pkgs, ... }:

let
  nixSystemPath = "/nix/var/nix/profiles/system";
  nixosRollbackAnyGen = pkgs.writeShellScriptBin "nixos-rollback-any-gen" ''
    nix-env --switch-generation $1 -p ${nixSystemPath}
    ${nixSystemPath}/bin/switch-to-configuration switch
  '';
  nerdFonts = pkgs.nerdfonts.override {
    fonts = [
      "FiraCode"
      "JetBrainsMono"
      "SourceCodePro"
    ];
  };
in
{
  environment = {
    homeBinInPath = true;
    pathsToLink = [ "/share/zsh" ];

    systemPackages = with pkgs; [
      curl
      git
      htop
      links2
      ncdu
      psmisc
      tree
      wget
      # Nix utils
      unstable.nh
      nix-du
      nix-index
      # Custom shell scripts
      nixosRollbackAnyGen
    ];
  };

  fonts = {
    enableDefaultPackages = true;
    fontDir.enable = true;
    packages = [
      nerdFonts
    ];
  };

  programs = {
    bash = {
      enableCompletion = true;
      enableLsColors = true;
    };

    command-not-found.enable = false;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    vim.defaultEditor = true;
    zsh.enable = true;
  };

  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  services = {
    openssh.enable = true;
    thermald.enable = true;
    uptimed.enable = true;
  };

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
      storageDriver = "overlay2";
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };
  };
}
