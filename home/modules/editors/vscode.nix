{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;

    extensions = with pkgs.vscode-extensions; [
      eamodio.gitlens
      jnoortheen.nix-ide
      arrterian.nix-env-selector
      scalameta.metals
      scala-lang.scala
      ms-vscode.theme-tomorrowkit
    ];

    userSettings = {
      "editor.fontSize" = 12;
      "editor.fontFamily" = "'Jetbrains Mono Nerd Font', 'monospace', monospace";
      "editor.fontLigatures" = true;
      "editor.renderWhitespace" = "boundary";
      "editor.formatOnPaste" = true;
      "editor.formatOnSave" = true;
      "files.autoSave" = "onFocusChange";
      "explorer.confirmDragAndDrop" = false;
      "workbench.startupEditor" = "none";
      "window.zoomLevel" = 2.5;
      "workbench.colorTheme" = "Tomorrow Night Eighties";
      "telemetry.telemetryLevel" = "off";
      "update.showReleaseNotes" = false;
      "security.workspace.trust.enabled" = false;
      "git.allowForcePush" = true;
      "gitlens.showWhatsNewAfterUpgrades" = false;
      "gitlens.showWelcomeOnInstall" = false;
      "nix.enableLanguageServer" = true;
      "update.mode" = "none";
    };
  };
}
