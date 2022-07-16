{ pkgs, ... }:

let
  enableWithZshIntegration = {
    enable = true;
    enableZshIntegration = true;
  };
  home-manager-rollback = with pkgs; writeShellScriptBin "home-manager-rollback"
    ''
      awk=${gawk}/bin/awk
      grep=${ripgrep}/bin/rg
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
  home = {
    packages = with pkgs; [
      _1password
      diskonaut
      home-manager-rollback
      httpie
      mdcat
      neofetch
      prettyping
      tealdeer
      wally-cli
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
    bat.enable = true;
    direnv = enableWithZshIntegration // {
      nix-direnv.enable = true;
    };
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
  };
}
