{

  profile = "personal";

  # Required by ZFS
  networking.hostId = "fcd4a364";

  boot = {
    initrd.availableKernelModules = [
      "ata_piix"
      "ohci_pci"
      "sd_mod"
      "sr_mod"
    ];
  };

  virtualisation.vmware.guest.enable = true;
}
