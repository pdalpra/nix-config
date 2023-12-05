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
      mkdir -p $out/share/fonts/opentype
      mkdir -p $out/share/fonts/truetype
      cp *.otf $out/share/fonts/opentype || true
      cp *.ttf $out/share/fonts/truetype || true
      runHook postInstall
    '';
  };
in
{
  home.packages = [
    myFonts
  ];
}
