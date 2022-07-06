{ lib, pkgs, ... }:

let
  onagre =
    pkgs.rustPlatform.buildRustPackage rec {
      pname = "onagre";
      version = "1.0.0-alpha.0";

      doCheck = false;

      src = pkgs.fetchFromGitHub {
        owner = "oknozor";
        repo = pname;
        rev = version;
        sha256 = "sha256-hP+slfCWgsTgR2ZUjAmqx9f7+DBu3MpSLvaiZhqNK1Q=";
      };

      cargoSha256 = "sha256-IOhAGrAiT2mnScNP7k7XK9CETUr6BjGdQVdEUvTYQT4=";

      nativeBuildInputs = with pkgs; [
        cmake
        pkg-config
      ];
      buildInputs = with pkgs; [
        freetype
        fontconfig
        xorg.libX11
        xorg.libXcursor
        xorg.libXrandr
        xorg.libXi
      ];
    };
  popLauncher =
    pkgs.rustPlatform.buildRustPackage rec {
      pname = "pop-launcher";
      version = "1.2.1";

      doCheck = false;

      src = pkgs.fetchFromGitHub {
        owner = "pop-os";
        repo = "launcher";
        rev = version;
        sha256 = "sha256-BQAO9IodZxGgV8iBmUaOF0yDbAMVDFslKCqlh3pBnb0=";
      };

      cargoSha256 = "sha256-cTvrq0fH057UIx/O9u8zHMsg+psMGg1q9klV5OMxtok=";

      buildAndTestSubdir = "bin";
      nativeBuildInputs = with pkgs; [
        cmake
        pkg-config
      ];

      installPhase = ''
        mkdir -p $out/bin
        cp /build/source/target/x86_64-unknown-linux-gnu/release/pop-launcher-bin $out/bin/pop-launcher'';

    };
in
{
  home.packages = with pkgs; [
    firefox
    rofi
    onagre
    popLauncher
  ];
  services.picom = {
    enable = true;
    fade = true;
    inactiveOpacity = "0.8";
  };

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      config = ./xmonad-config/config.hs;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.monad-logger
      ];

    };
  };
}
