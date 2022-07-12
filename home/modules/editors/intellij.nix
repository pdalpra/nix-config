{ pkgs, ... }:

let
  ideaPackage = pkgs.unstable.jetbrains.idea-ultimate;
  ideaAlias = pkgs.writeShellScriptBin "idea" "${ideaPackage}/bin/idea-ultimate";
in
{
  home.packages = [
    ideaPackage
    ideaAlias
  ];
}
