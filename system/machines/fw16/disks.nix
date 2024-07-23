_:

let
  mainDisk = "/dev/nvme0n1";
  swapSize = "100G";
  pool = "main";
in
{
  services.zfs.trim.enable = true;
  system.impermanence.zfs = {
    enable = true;
    inherit pool;
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
            size = "2G";
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
      options.ashift = "13"; # 8k blocks
    };
  };
}
