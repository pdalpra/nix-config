{ pkgs, ... }:

let enableWithZshIntegration = {
  enable = true;
  enableZshIntegration = true;
};
in
{
  home.packages = with pkgs; [
    _1password
    diskonaut
    du-dust
    duf
    hyperfine
    mdcat
    neofetch
    prettyping
    ripgrep
    tealdeer
    tokei
    wally-cli
  ];

  programs = {
    bat.enable = true;
    direnv = enableWithZshIntegration;
    exa = {
      enable = true;
      enableAliases = true;
    };
    fzf = enableWithZshIntegration;
    jq.enable = true;
    nix-index = enableWithZshIntegration;
    nnn = {
      enable = true;
      bookmarks = {
        w = "~/Work";
      };
    };
    zoxide = enableWithZshIntegration // {
      options = ["--cmd cd"];
    };

    ############
    # STARSHIP #
    ############

    starship = enableWithZshIntegration // {
      package = pkgs.unstable.starship;
    };

    #######
    # ZSH #
    #######

    zsh = {
      enable = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      autocd = true;

      envExtra = ''
        DOCKER_SCAN_SUGGEST=false
      '';

      prezto = {
        enable = true;
        caseSensitive = false;
        editor.dotExpansion = true;
        pmodules = [
          "environment"
          "terminal"
          "editor"
          "directory"
          "spectrum"
          "syntax-highlighting"
          "history-substring-search"
          "autosuggestions"
          "completion"
        ];
        syntaxHighlighting.highlighters = [
          "main"
          "brackets"
          "regexp"
          "root"
        ];
        utility.safeOps = false;
      };

      plugins = [
        {
          # Prepend sudo to the current or last command with <ESC><ESC>
          name = "sudo";
          src = pkgs.fetchFromGitHub {
            owner  = "hcgraf";
            repo   = "zsh-sudo";
            rev    = "d8084def6bb1bde2482e7aa636743f40c69d9b32";
            sha256 = "sha256-I17u8qmYttsodD58PqtTxtVZauyYcNw1orFLPngo9bY=";
          };
        }
      ];

      shellAliases = {
        cat  = "${pkgs.bat}/bin/bat";
        rcat = "${pkgs.bat}/bin/bat -pP";
        grep = "${pkgs.ripgrep}/bin/rg";
        time = "${pkgs.hyperfine}/bin/hyperfine";
        cloc = "${pkgs.tokei}/bin/tokei";
        du   = "${pkgs.du-dust}/bin/dust";
        df   = "${pkgs.duf}/bin/duf";
      };
    };
  };
}