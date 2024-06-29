{ pkgs, lib, ... }:

{
  home.activation = {
    # Remove sayings on logout, installed by prezto
    removeZlogout = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm -f $HOME/.zlogout
    '';
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    syntaxHighlighting = {
      enable = true;
      catppuccin.enable = true;
    };
    autocd = true;

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
  };
}
