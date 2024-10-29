{ config, lib, myLib, pkgs, ... }:

let
  pidFile = "/var/run/illuminanced.pid";
  cfg = config.services.illuminanced;
  # TODO: work on my fork to allow tweaking the keycode to switch modes
  illuminancedPkg = pkgs.rustPlatform.buildRustPackage rec {
    pname = "illuminanced";
    version = "0.0.1";
    doCheck = false;
    cargoHash = "sha256-+2AQRaIxPGOmu0AHxkcd6LAHcAYE2X5MZB9MkIBzj2A=";
    src = pkgs.fetchFromGitHub {
      owner = "pdalpra";
      repo = pname;
      rev = "93dd5c2b5de5a77abd7ea486cb663113cb79492d";
      hash = "sha256-PQHGKz/2UxCf8FosuxmL7DL9Z+H9nzXHdyn+73gWw1I=";
    };
  };
  levels = myLib.mergeAll (lib.lists.imap0
    (i: v: {
      "illuminance_${toString i}" = v.illuminance;
      "light_${toString i}" = v.light;
    })
    cfg.brightness.levels);
  configFile = (pkgs.formats.toml { }).generate "illuminance.toml" {
    daemonize = {
      log_to = cfg.log.file;
      log_level = cfg.log.level;
      pid_file = pidFile;
    };
    general = {
      light_steps = 10;
      min_backlight = cfg.brightness.min;
      step_barrier = 0.1;
      enable_max_brightness_mode = cfg.brightness.enableMaxBrightnessMode;

      check_period_in_seconds = cfg.interval;
      backlight_file = cfg.devices.current;
      max_backlight_file = cfg.devices.max;
      illuminance_file = cfg.devices.illuminance;
      event_device_mask = cfg.devices.events.mask;
      event_device_name = cfg.devices.events.name;
    };
    kalman = {
      inherit (cfg.kalman) q r covariance;
    };
    light = {
      points_count = builtins.length cfg.brightness.levels;
    } // levels;
  };
in
{
  # TODO: document module options
  # - kalman
  # - devices.events
  options.services.illuminanced = with lib; with types; {
    enable = mkEnableOption "Enable illuminanced";
    package = mkOption {
      type = package;
      description = "The package used to install illuminanced";
      default = illuminancedPkg;
    };
    log = {
      file = mkOption {
        type = str;
        default = "syslog";
        description = "'syslog' or path to a file";
      };
      level = mkOption {
        type = enum [ "OFF" "ERROR" "WARN" "INFO" "DEBUG" "TRACE" ];
        default = "ERROR";
        description = "Log level to use";
      };
    };
    interval = mkOption {
      type = int;
      default = 1;
      description = "Interval for checking the ambient light sensor";
    };
    kalman = {
      q = mkOption {
        type = int;
        default = 1;
        description = "TODO documentation ";
      };
      r = mkOption {
        type = int;
        default = 20;
        description = "TODO documentation";
      };
      covariance = mkOption {
        type = int;
        default = 10;
        description = "TODO documentation";
      };
    };
    devices = {
      current = mkOption {
        type = str;
        default = "/sys/class/backlight/amdgpu_bl2/brightness";
        description = "Path to device file with the current brightness level";
      };
      max = mkOption {
        type = str;
        default = "/sys/class/backlight/amdgpu_bl2/max_brightness";
        description = "Path to device file with the max brightness level";
      };
      illuminance = mkOption {
        type = str;
        default = "/sys/bus/iio/devices/iio:device0/in_illuminance_raw";
        description = "Path to device file with the detected illuminance";
      };
      events = {
        mask = mkOption {
          type = str;
          default = "/dev/input/event*";
          description = "TODO documentation";
        };
        name = mkOption {
          type = str;
          default = "Framework Laptop 16 Keyboard Module - ISO Keyboard";
          description = "TODO documentation";
        };
      };
    };
    brightness = {
      enableMaxBrightnessMode = mkEnableOption "Enable max brightness mode";
      min = mkOption {
        type = int;
        default = 30;
        description = "Minimum backlight level";
      };

      levels = mkOption {
        type = listOf (submodule {
          options = {
            illuminance = mkOption {
              type = int;
              description = "Illuminance level triggering the brightness change";
            };
            light = mkOption {
              type = int;
              description = "number of light steps matching to use for this level";
            };
          };
        });
        description = "Steps for increasing brightness based on illuminance";
        default = [
          { illuminance = 0; light = 0; }
          { illuminance = 20; light = 1; }
          { illuminance = 300; light = 3; }
          { illuminance = 700; light = 4; }
          { illuminance = 1100; light = 5; }
          { illuminance = 7100; light = 10; }
        ];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.illuminanced = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      description = "Automatic brightness manager";
      serviceConfig = {
        Type = "forking";
        ExecStart = "${illuminancedPkg}/bin/illuminanced -c ${configFile}";
        PIDFile = pidFile;
        Restart = "on-failure";
      };
    };
  };
}
