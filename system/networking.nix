{ lib, ... }:
{
  services.resolved = {
    enable = true;
    dnsovertls = "opportunistic";
  };

  networking = {
    firewall.enable = false;
    useDHCP = lib.mkDefault true;
    nameservers = [
      "9.9.9.9"
      "1.1.1.1"
    ];
    timeServers = [
      "0.pool.ntp.org"
      "1.pool.ntp.org"
    ];
  };
}
