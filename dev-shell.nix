{ system, nixpkgs }:
let pkgs = nixpkgs.legacyPackages.${system};
in
pkgs.mkShell {
  name = "nix-config-dev";
  buildInputs = with pkgs; [
    cachix
    nixfmt
    nix-linter
  ];
}