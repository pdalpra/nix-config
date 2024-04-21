{ pkgs, ... }:

{
  programs = {
    chromium = {
      enable = true;
      package = pkgs.chromium;
      extensions = [
        { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } # 1Password
        { id = "mdjildafknihdffpkfmmpnpoiajfjnjd"; } # Consent-o-matic
        { id = "mjdepdfccjgcndkmemponafgioodelna"; } # Distraction Free Youtube
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # uBlock Origin
        { id = "fgmjlmbojbkmdpofahffgcpkhkngfpef"; } # Startpage
      ];
    };

    firefox = {
      enable = true;

      profiles = {
        default = {
          id = 0;
          settings = {
            "app.update.auto" = false;
          };
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            consent-o-matic
            df-youtube
            onepassword-password-manager
            ublock-origin
            startpage-private-search
          ];
        };
      };
    };
  };
}
