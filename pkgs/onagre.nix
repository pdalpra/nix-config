{ pkg-config, cmake, freetype, fontconfig, rustPlatform, fetchFromGitHub }:
rustPlatform.buildRustPackage rec {

  pname = "onagre";
  version = "1.0.0-alpha.0";

  src = fetchFromGitHub {
    owner = "oknozor";
    repo = pname;
    rev = version;
    sha256 = "hP+slfCWgsTgR2ZUjAmqx9f7+DBu3MpSLvaiZhqNK1Q=";
  };

  cargoSha256 = "IOhAGrAiT2mnScNP7k7XK9CETUr6BjGdQVdEUvTYQT4=";

  nativeBuildInputs = [
    pkg-config
    cmake
  ];

  buildInputs = [
    freetype
    fontconfig
  ];
}
