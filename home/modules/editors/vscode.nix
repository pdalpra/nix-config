{ pkgs, ... }:

let
  myDefaults =
    pkgs.vscode-utils.buildVscodeExtension {
      name = "pdalpra.vscode-defaults";
      vscodeExtUniqueId = "vscode-defaults";
      vscodeExtName = "My Defaults";
      vscodeExtPublisher = "pdalpra";
      version = "0.0.1";
      src = pkgs.fetchFromGitHub {
        owner = "pdalpra";
        repo = "vscode-defaults";
        rev = "919c61806df97badbfabaa2ee4fcbeb0343e9b27";
        sha256 = "sha256-p2LKmNuKwXy8FTpTX+SJSLIAFyFqNIXz/FHDsK65a5I=";
      };
    };
in
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
      ms-vscode.theme-tomorrowkit
      myDefaults
    ];
  };
}
