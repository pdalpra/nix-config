{ config, lib, pkgs, ... }:

with lib;

let
  pkgRev = "93dd5c2b5de5a77abd7ea486cb663113cb79492d";
  cfg = config.services.illuminanced;
  illuminancedPkg = pkgs.rustPlatform.buildRustPackage rec {
    pname = "illuminanced";
    version = "0.1-${pkgRev}";
    doCheck = false;
    cargoHash = "sha256-uhQ7B/mki4xouftz2vBzF/DeMql/9sEkfBMVEHG2adE=";
    src = pkgs.fetchFromGitHub {
      owner = "pdalpra";
      repo = pname;
      rev = pkgRev;
      hash = "sha256-PQHGKz/2UxCf8FosuxmL7DL9Z+H9nzXHdyn+73gWw1I=";
    };
  };
  configFile = (pkgs.formats.toml { }).generate "illuminance.toml" {
    daemonize = {
      log_to = cfg.log.file;
      log_level = cfg.log.level;
      pid_file = cfg.pidFile;
    };
    general = {
      check_period_in_seconds = cfg.interval;
    };
    kalman = {
      inherit (cfg.kalman) q r covariance;
    };
    light = {
      points_count = builtins.length cfg.levels;
    };
  };
in
{
  options.services.illuminanced = with types; {
    enable = mkEnableOption "Enable impermanence";
    package = mkOption {
      type = package;
      description = "The package used to install illuminanced";
      default = illuminancedPkg;
    };
    pidFile = mkOption {
      type = string;
      default = "/var/run/illuminanced.pid";
      description = "Path to PID file";
    };
    log = {
      file = mkOption {
        type = string;
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
        description = "TODO";
      };
      r = mkOption {
        type = int;
        default = 20;
        description = "TODO";
      };
      covariance = mkOption {
        type = int;
        default = 10;
        description = "TODO";
      };
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

  config = mkIf cfg.enable {
    systemd.services.illuminanced = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      description = "Automatic brightness manager";
      requires = [ "syslog.socket" ];
      serviceConfig = {
        Type = "forking";
        ExecStart = "${illuminancedPkg}/bin/illuminanced -c ${configFile}";
        PIDFile = "/var/run/illuminanced.pid";
        Restart = "on-failure";
      };
    };
  };
}
