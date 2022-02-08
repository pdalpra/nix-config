{
  boot = {
    initrd.availableKernelModules = [ "ata_piix" "ohci_pci" "sd_mod" "sr_mod" ];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };
    "/mnt/virtualbox" = {
      device  = "host-home";
      fsType  = "vboxsf";
      options = ["auto,nofail,rw"];
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  networking.interfaces.enp0s3.useDHCP = true;
  virtualisation.virtualbox.guest.enable = true;
}
