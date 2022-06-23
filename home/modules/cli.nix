{ pkgs, lib, ... }:

let
  enableWithZshIntegration = {
    enable = true;
    enableZshIntegration = true;
  };
  home-manager-rollback = pkgs.writeShellScriptBin "home-manager-rollback"
    ''
      awk=${pkgs.gawk}/bin/awk
      grep=${pkgs.ripgrep}/bin/rg
      if [ -z $1 ]; then
        generation=$(home-manager generations | $awk 'NR==2')
      else
        generation=$(home-manager generations | $grep "id $1\s+")
      fi
      generation_path=$(echo $generation | $awk '{print $NF}')
      echo "Rollback home-manager to $generation_path"
      $generation_path/activate  
    '';
in
{
  home.packages = with pkgs; [
    _1password
    diskonaut
    du-dust
    duf
    hyperfine
    home-manager-rollback
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
      options = [ "--cmd cd" ];
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
            owner = "hcgraf";
            repo = "zsh-sudo";
            rev = "d8084def6bb1bde2482e7aa636743f40c69d9b32";
            sha256 = "sha256-I17u8qmYttsodD58PqtTxtVZauyYcNw1orFLPngo9bY=";
          };
        }
      ];

      shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
        rcat = "${pkgs.bat}/bin/bat -pP";
        grep = "${pkgs.ripgrep}/bin/rg";
        time = "${pkgs.hyperfine}/bin/hyperfine";
        cloc = "${pkgs.tokei}/bin/tokei";
        du = "${pkgs.du-dust}/bin/dust";
        df = "${pkgs.duf}/bin/duf";
        nixdev = "nix develop --command zsh";
      };
    };
  };
}
