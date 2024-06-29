{ pkgs, config, ... }:


let
  gitConfigHome =
    "${config.xdg.configHome}/git";
in
{
  age.secrets = {
    perso = {
      file = ../../../secrets/git-perso.age;
      path = "${gitConfigHome}/perso";
    };
    work = {
      file = ../../../secrets/git-work.age;
      path = "${gitConfigHome}/work";
    };
  };


  home.file.".tigrc" = {
    text = ''
      bind main = !git commit --fixup=%(commit)
      bind generic K move-up
      bind generic J move-down
    '';
  };

  home.packages = with pkgs; [
    git-crypt
    tig
  ];

  programs = {
    gh = {
      enable = true;
      settings = {
        git_protocol = "ssh";
        aliases = {
          clone = "repo clone";
          prv = "pr view --web";
          prc = "pr create --fill";
        };
      };
    };

    git = {
      enable = true;
      includes = [
        {
          inherit (config.age.secrets.perso) path;
        }
        {
          condition = "gitdir:~/Code/work";
          inherit (config.age.secrets.work) path;
        }
      ];
      delta = {
        enable = true;
        catppuccin.enable = true;
        options = {
          side-by-side = true;
        };
      };
      aliases = {
        st = "status";
        co = "checkout";
        br = "branch";
        ci = "commit";

        tree = ''!git log --graph --all --pretty=oneline --graph --decorate --color=always | less -r'';
        cleanup = ''!git remote prune origin && git branch --merged | grep -v '^* master$' | grep -v '^  master$' | xargs -r git branch -d'';
        wip = ''!git ci -a --amend --no-edit --date=now && git push -f'';
      };
      extraConfig = {
        color.ui = true;
        core = {
          autocrlf = "input";
          editor = "vim";
          whitespace = "blank-at-eol, -blank-at-eof";
          compression = 0;
        };
        log = {
          date = "rfc2822";
          abbrevCommit = true;
        };
        push = {
          default = "current";
          followTags = true;
        };
        rebase = {
          autosquash = true;
          autostash = true;
        };
        sequence.editor = "${pkgs.git-interactive-rebase-tool}/bin/interactive-rebase-tool";
        help.autocorrect = 1;
        credential.helper = "store --file ~/.gitcredentials";
        init.defaultBranch = "main";
        pull = {
          rebase = true;
          ff = true;
        };
        url = {
          "https://github.com/".insteadOf = "gh:";
          "ssh://git@github.com".pushInsteadOf = "gh:";
        };
      };
      ignores = [
        # Mac OS X files
        ".DS_Store"
        # IDE-specific files
        "*.iml"
        ".idea"
        ".vscode"
        # Scala Metals files
        ".bsp"
        ".bloop"
        ".metals"
        "metals.sbt"
      ];
    };
  };
}
