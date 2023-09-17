{ pkgs, ... }:

let
  myFonts = pkgs.stdenvNoCC.mkDerivation {
    pname = "my-fonts";
    version = "1.0.0";
    src = ../fonts;
    dontConfigure = true;

    meta = {
      description = "My fonts";
    };

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/fonts
      cp -R opentype $out/share/fonts
      runHook postInstall
    '';
  };
in
{
  home.packages = [
    myFonts
  ];
}
