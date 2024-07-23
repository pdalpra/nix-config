{ config, lib, ... }:

with lib;

let
  cfg = config.hardware.laptop;
  powerUnit = description: {
    enable = true;
    inherit description;
    unitConfig = {
      DefaultDependencies = "no";
      StopWhenUnneeded = "yes";
    };
  };
  mkProfileOption = description: mkOption {
    inherit description;
    default = "balanced";
    type = types.enum [
      "performance"
      "balanced"
      "power-saver"
    ];
  };
in
{
  options.hardware.battery = with types; {
    enable = mkEnableOption "Enable laptop-related tooling";
    powerProfiles = {
      enable = mkEnableOption "Enable control of power-profiles-daemon";
      charging = mkProfileOption "Profile to use on A/C";
      discharging = mkProfileOption "Profile to use on battery";
      low = mkProfileOption "Profile to use on low battery";
      critical = mkProfileOption "Profile to use on critcal battery";
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      systemd.targets = {
        power-charging = powerUnit "Actions when plugged into A/C";
        power-discharging = powerUnit "Actions when running on battery";
        power-low = powerUnit "Actions when running on low battery";
        power-critical = powerUnit "Actions when running on critical battery";
      };

      services.udev.extraRules = ''
        SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-${cfg.batterCritical}]", ENV{SYSTEMD_WANTS}=""
        SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[${cfg.batterCritical}-${cfg.batteryLow}", ENV{SYSTEMD_WANTS}=
      '';
    })
    (mkIf (cfg.enable && cfg.powerProfiles.enable) { })
  ];

}
