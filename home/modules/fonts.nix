{ config, lib, .. }:

let
  home = config.home.homeDirectory;
  fontNames = [
    "BerkeleyMono-Bold.otf"
    "BerkeleyMono-BoldItalic.otf"
    "BerkeleyMono-Italic.otf"
    "BerkeleyMono-Regular.otf"
    "BerkeleyMono-Bold.ttf"
    "BerkeleyMono-BoldItalic.ttf"
    "BerkeleyMono-Italic.ttf"
    "BerkeleyMono-Regular.ttf"
  ];
  toSecret = fontName: {
    age.secrets.${fontName} = {
      file = ../../secrets/fonts/${fontName}.age;
      path = "${home}/.local/share/fonts/${fontName}";
    };
  };
in
lib.mergeAll (map toSecret fontNames)
