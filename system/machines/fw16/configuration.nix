{ pkgs, lib, config, hardware, ... }:

{
  profile = "personal";

  imports = [
    hardware.framework-16-7040-amd
  ];

  boot = {
    zfs.package = pkgs.unstable.zfs;
    kernelPackages = pkgs.unstable.linuxPackages_6_8;
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    opengl = with pkgs; {
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = [ amdvlk ];
      extraPackages32 = [ driversi686Linux.amdvlk ];
    };
  };

  environment.systemPackages = with pkgs; [
    vulkan-tools
    clinfo
    glxinfo
    powertop
    nvtopPackages.amd
    lm_sensors
  ];

  hardware = {
    enableAllFirmware = true;
    bluetooth.enable = true;
    openrazer = {
      enable = true;
      users = lib.attrNames config.users.users;
    };
  };

  networking = {
    hostId = "6446c0d6"; # Required by ZFS
    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
  };

  services = {
    blueman.enable = true;
    fwupd.enable = true;
    udisks2.enable = true;
    thermald.enable = true;
    gvfs.enable = true;
    illuminanced.enable = true;
  };

  programs = {
    light.enable = true;
    nm-applet.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      gamescopeSession.enable = true;
    };
  };

  system.impermanence = {
    directories = [
      "/var/lib/bluetooth"
      "/etc/NetworkManager/system-connections"
    ];
  };

}
