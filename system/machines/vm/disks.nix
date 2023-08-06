let
  swapSize = "10G";
in
{
  disko.devices = {
    disk.sda = {
      device = "/dev/sda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            name = "ESP";
            device = "/dev/disk/by-label/boot";
            end = "512M";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          root = {
            name = "root";
            device = "/dev/disk/by-label/nixos";
            start = "512M";
            end = "-${swapSize}";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
            };
          };
          swap = {
            name = "swap";
            device = "/dev/disk/by-label/swap";
            size = "100%";
            content = {
              type = "swap";
              resumeDevice = true;
            };
          };
        };
      };
    };
  };
}
