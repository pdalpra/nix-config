{ config, ... }:

let
  mainDisk = "/dev/sda";
  swapSize = "4G";
  pool = "main";
in
{
  services.zfs.trim.enable = true;
  system.impermanence.zfs = {
    enable = true;
    inherit pool;
  };

  boot = {
    supportedFilesystems = [ "zfs" ];
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  };

  disko.devices = {
    disk.main = {
      device = mainDisk;
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            name = "ESP";
            type = "EF00";
            size = "1G";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          luks = {
            end = "-${swapSize}";
            content = {
              type = "luks";
              name = "encrypted";
              extraOpenArgs = [ "--allow-discards" ];
              content = {
                type = "zfs";
                inherit pool;
              };
            };
          };
          swap = {
            size = "100%";
            content = {
              type = "swap";
              resumeDevice = true;
            };
          };
        };
      };
    };
    zpool.${pool} = {
      type = "zpool";
      mode = ""; # unmirrored
      options.ashift = "12"; # 4k blocks
    };
  };
}
