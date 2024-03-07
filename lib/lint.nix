{ pkgs, lintingPkgs }:

pkgs.stdenvNoCC.mkDerivation {
  name = "lint";
  dontBuild = true;
  doCheck = true;
  src = ../.;
  nativeBuildInputs = lintingPkgs;
  checkPhase = ''
    treefmt --fail-on-change --no-cache &&\
    statix check &&\
    deadnix -f
  '';
  installPhase = ''
    mkdir "$out"
  '';

}
