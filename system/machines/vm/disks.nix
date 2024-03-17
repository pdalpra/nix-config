{ config, lib, ... }:

let
  mainDisk = "/dev/sda";
  swapSize = "4G";
  blankSnapshot = "main/root@blank";
  poolName = "main";
  impermanencePaths = config.system.impermanence.paths;
  zfs_fs = mountpoint: options: {
    inherit mountpoint;
    type = "zfs_fs";
    options.mountpoint = "legacy";
  } // options;
in
{
  services.zfs.trim.enable = true;

  fileSystems = {
    ${impermanencePaths.system}.neededForBoot = true;
    ${impermanencePaths.homes}.neededForBoot = true;
  };

  boot = {
    supportedFilesystems = [ "zfs" ];
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    initrd.postDeviceCommands = lib.mkAfter ''
      zfs rollback -r ${blankSnapshot} && echo "Blank snapshot restored"
    '';
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
                pool = poolName;
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
    zpool.${poolName} = {
      type = "zpool";
      mode = ""; # unmirrored
      options.ashift = "13"; # 8k blocks
      rootFsOptions.canmount = "off";
      datasets = {
        root = zfs_fs "/" {
          postCreateHook = "zfs snapshot ${blankSnapshot}";
        };
        nix = zfs_fs "/nix" { };
        persistentSystem = zfs_fs impermanencePaths.system { };
        persistentHomes = zfs_fs impermanencePaths.homes { };
      };
    };
  };
}
