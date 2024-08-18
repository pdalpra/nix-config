{ pkgs, lib, myLib, ... }:


let
  isRust = path: lib.hasSuffix ".rs" path && path != "mod.rs";
  toogleModules = enabled: modules: myLib.mergeAll (map (mod: { "${mod}" = { disabled = !enabled; }; }) modules);
  starshipPackage = pkgs.unstable.starship;
  promptOrder = [
    "nix_shell"
    "directory"
    "git_branch"
    "git_commit"
    "git_state"
    "git_status"
    "rust"
    "java"
    "elm"
    "golang"
    "python"
    "nodejs"
    "haskell"
    "jobs"
    "line_break"
    "character"
  ];
  promptFormat = lib.concatStrings (map (s: "\$${s}") promptOrder);
  modulesSources = "${starshipPackage.src}/src/modules";
  enabledModules = toogleModules true promptOrder; # <== ensure all modules used in the prompt are enabled

  # Disabling all modules by default
  # - Grab starship's sources
  # - List all Rust sources files
  # - Get the file name, without the extension (<- name of the module)
  # - Exclude the enabled modules
  disabledModules = lib.pipe modulesSources [
    (myLib.filterFiles isRust)
    (map (lib.removeSuffix ".rs"))
    (lib.subtractLists promptOrder)
    (toogleModules false)
  ];
  starshipConfig = {
    format = promptFormat;
    directory = {
      format = "\\[[$path](bold fg:39)\\]";
      truncation_length = 4;
      truncation_symbol = "…/";
    };
    elm.format = "\\[[elm: $version](bold fg:33)\\]";
    git_branch = {
      format = "\\[[$symbol$branch](bold fg:40)\\]";
      truncation_length = 20;
    };
    git_commit = {
      format = "[\\( $hash$tag\\)](bold white)";
      tag_disabled = false;
      tag_symbol = "";
    };
    git_status = {
      ahead = "⇡$count";
      behind = " ⇣$count";
      untracked = "?$count";
      stashed = "*$count";
      modified = "!$count";
      staged = "+$count";
      renamed = "->$count";
      deleted = "☓$count";
      style = "bold fg:136";
    };
    golang.format = "\\[[go: $version](bold fg:14)\\]";
    haskell.format = "\\[[λ: $version](bold fg:5)\\]";
    python.format = "\\[[python: $version](bold fg:2)\\]";
    nodejs.format = "\\[[node: $version](bold fg:11)\\]";
    java.format = "\\[[java: $version](bold fg:202)\\]";
    jobs.symbol = "";
    nix_shell.format = "\\[[nix: $name](bold fg:69)\\]";
    rust.format = "\\[[rust: $version](bold fg:1)\\]";
  };
in
{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    catppuccin.enable = true;
    package = starshipPackage;
    settings = myLib.mergeAll [
      enabledModules
      disabledModules
      starshipConfig
    ];
  };
}
