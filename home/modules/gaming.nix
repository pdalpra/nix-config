{ pkgs }:

{
  home.packages = with pkgs; [
    steam
    steam-run
  ];

  programs.mangohud = {
    enable = true;
    enableSessionWide = true;
  };

}
