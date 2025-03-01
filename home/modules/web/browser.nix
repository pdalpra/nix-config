{ pkgs, ... }:

{
  programs = {
    chromium = {
      enable = true;
      package = pkgs.brave;
      extensions = [
        { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } # 1Password
        { id = "mdjildafknihdffpkfmmpnpoiajfjnjd"; } # Consent-o-matic
        { id = "mjdepdfccjgcndkmemponafgioodelna"; } # Distraction Free Youtube
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # uBlock Origin
        { id = "fgmjlmbojbkmdpofahffgcpkhkngfpef"; } # Startpage
      ];
    };
  };
}
