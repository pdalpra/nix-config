{ lib, config, hardwareProfiles, ... }: {

  imports = [
    hardwareProfiles.framework-13-7040-amd
  ];

  hardware = {
    enableAllFirmware = true;
    bluetooth.enable = true;
    keyboard.qmk.enable = true;
    openrazer = {
      enable = true;
      users = lib.attrNames config.users.users;
    };
  };

  systemd.network = {
    enable = true;
    wait-online.enable = false;
  };

  networking = {
    hostId = "6446c0d6"; # Required by ZFS
    networkmanager = {
      enable = true;
      insertNameservers = [
        "1.1.1.1"
        "9.9.9.9"
        "4.4.4.4"
        "8.8.8.8"
      ];
    };
  };

  services = {
    fwupd.enable = true;
  };

  system.impermanence = {
    directories = [
      "/var/lib/bluetooth"
      "/etc/NetworkManager/system-connections"
    ];
  };

}
