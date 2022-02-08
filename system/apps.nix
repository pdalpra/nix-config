{ pkgs, ... }:

let
  nixSystemPath = "/nix/var/nix/profiles/system";
  nixosListGens = pkgs.writeShellScriptBin "nixos-list-gens"
    ''nix-env -p ${nixSystemPath} --list-generations'';
  nixosRollbackAnyGen = pkgs.writeShellScriptBin "nixos-rollback-any-gen" ''
    nix-env --switch-generation $1 -p ${nixSystemPath}
    ${nixSystemPath}/bin/switch-to-configuration switch
  '';
  doasSudo = pkgs.writeShellScriptBin "sudo" "doas $1";
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
      links
      ncdu
      tree
      wget
      # Nix utils
      nix-du
      nix-index
      # Custom shell scripts
      doasSudo
      nixosListGens
      nixosRollbackAnyGen
    ];
  };

  fonts = {
    fontDir.enable = true;
    fonts = [ nerdFonts ];
  };

  programs = {
    bash = {
      enableCompletion = true;
      enableLsColors   = true;
    };

    command-not-found.enable = false;

    vim.defaultEditor = true;
    zsh.enable        = true;
  };

  security = {
    doas = {
      enable = true;
      wheelNeedsPassword = false;
    };

    sudo.enable = false;
  };

  services = {
    openssh.enable = true;
    uptimed.enable = true;
  };

  virtualisation = {
    docker = {
      enable        = true;
      enableOnBoot  = true;
      storageDriver = "overlay2";
      autoPrune = {
        enable = true;
        dates  = "weekly";
      };
    };
  };
}