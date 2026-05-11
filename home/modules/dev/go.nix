{ pkgs, ... }:

{
  programs.go = {
    enable = true;
    package = pkgs.unstable.go;
    env.GOPATH = "Code/go";
  };
}
