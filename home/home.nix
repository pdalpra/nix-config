{ ... }:

{
  imports = (import ./modules);

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  news.display = "silent";

  # After initial install, let home-manager manage itself
  programs.home-manager.enable = true;
}
