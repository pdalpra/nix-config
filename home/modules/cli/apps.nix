{ pkgs, ... }:

let
  enableWithZshIntegration = {
    enable = true;
    enableZshIntegration = true;
  };
in
{
  home = {
    packages = with pkgs; [
      _1password
      comma
      diskonaut
      httpie
      manix
      mdcat
      neofetch
      prettyping
      rsync
      tealdeer
    ];

    sessionVariables = {
      DIRENV_LOG_FORMAT = ""; # disable direnv's logs
    };

    shellAliases = with pkgs; {
      cat = "${bat}/bin/bat";
      rcat = "${bat}/bin/bat -pP";
      grep = "${ripgrep}/bin/rg";
      time = "${hyperfine}/bin/hyperfine";
      cloc = "${tokei}/bin/tokei";
      du = "${du-dust}/bin/dust";
      df = "${duf}/bin/duf";
      nixdev = "nix develop --command zsh";
    };
  };

  programs = {
    bat = {
      enable = true;
      catppuccin.enable = true;
    };
    bottom = {
      enable = true;
      catppuccin.enable = true;
    };
    direnv = enableWithZshIntegration // {
      nix-direnv.enable = true;
    };
    eza = enableWithZshIntegration;
    jq.enable = true;
    nix-index = enableWithZshIntegration;
    nnn = {
      enable = true;
      bookmarks = {
        w = "~/Code";
      };
    };
    zoxide = enableWithZshIntegration // {
      options = [ "--cmd cd" ];
    };
  };
}
