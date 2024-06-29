{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    package = pkgs.unstable.vscode;

    extensions = with pkgs.vscode-extensions; [
      eamodio.gitlens
      jnoortheen.nix-ide
      arrterian.nix-env-selector
      scalameta.metals
      scala-lang.scala
      catppuccin.catppuccin-vsc
      catppuccin.catppuccin-vsc-icons
    ];
  };
}
