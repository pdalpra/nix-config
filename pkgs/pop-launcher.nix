{ cmake, pkg-config, rustPlatform, cargo, fetchFromGitHub }:
rustPlatform.buildRustPackage rec {

  pname = "pop-launcher";
  version = "1.2.1";

  src = fetchFromGitHub {
    owner = "pop-os";
    repo = "launcher";
    rev = version;
    sha256 = "sha256-BQAO9IodZxGgV8iBmUaOF0yDbAMVDFslKCqlh3pBnb0";
  };
  
  cargoSha256 = "cTvrq0fH057UIx/O9u8zHMsg+psMGg1q9klV5OMxtok=";

  buildAndTestSubdir = "bin";
  nativeBuildInputs = [
    cmake
    pkg-config
  ];

}
