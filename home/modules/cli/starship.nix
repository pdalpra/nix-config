{ pkgs, lib, ... }:

with lib;
with builtins;

let
  isRustFile = path: type:
    hasSuffix ".rs" path && type == "regular" && path != "mod.rs";
  mergeAll = foldl' recursiveUpdate { };
  disableModules =
    disabled: modules: mergeAll (map (mod: { "${mod}" = { inherit disabled; }; }) modules);

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
    "jobs"
    "line_break"
    "character"
  ];
  promptFormat = concatStrings (map (s: "\$${s}") promptOrder);
  modulesSources = readDir "${starshipPackage.src}/src/modules";
  enabledModules = disableModules false promptOrder; # <== ensure all modules used in the prompt are enabled

  # Disabling all modules by default
  # - Grab starship's sources
  # - List all Rust sources files
  # - Get the file name, without the extension (<- name of the module)
  # - Exclude the enabled modules
  disabledModules = pipe modulesSources [
    (filterAttrs isRustFile)
    attrNames
    (map (removeSuffix ".rs"))
    (subtractLists promptOrder)
    (disableModules true)
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
    package = starshipPackage;
    settings = mergeAll [
      enabledModules
      disabledModules
      starshipConfig
    ];
  };
}
