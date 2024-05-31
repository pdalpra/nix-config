{ config, lib, pkgs, ... }:

let
  cfg = config.services.power-profiles-daemon-custom;
in

{

  ###### interface

  options = {

    services.power-profiles-daemon-custom = {

      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether to enable power-profiles-daemon, a DBus daemon that allows
          changing system behavior based upon user-selected power profiles.
        '';
      };

      package = lib.mkPackageOption pkgs "power-profiles-daemon" { };

    };

  };


  ###### implementation

  config = lib.mkIf cfg.enable {

    environment.systemPackages = [ cfg.package ];

    services = {
      power-profiles-daemon.enable = lib.mkForce false;
      tlp.enable = lib.mkForce false;
      dbus.packages = [ cfg.package ];
      udev.packages = [ cfg.package ];
    };

    systemd.packages = [ cfg.package ];


  };

}