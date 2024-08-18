{ config, lib, myLib, ... }:

let
  cfg = config.system.impermanence;
  concat = s1: s2: s1 + s2;
  perUser = user: {
    "${user.name}" = {
      inherit (user) files;
      directories =
        (map (concat ".config/") user.xdg.configs) ++
        (map (concat ".local/share/") user.xdg.data) ++
        (map (concat ".local/state/") user.xdg.states) ++
        (map (concat ".cache/") user.xdg.caches) ++
        user.directories;
    };
  };
  blankSnapshot = poolName: "${poolName}/root@blank";
  zfs_fs = mountpoint: options: {
    inherit mountpoint;
    type = "zfs_fs";
    options.mountpoint = "legacy";
  } // options;
in
{
  options.system.impermanence = with lib; with types; {
    enable = mkEnableOption "Enable impermanence";
    pause = mkEnableOption "Keep disk layout, but do not wipe on boot";

    zfs = {
      enable = mkEnableOption "Use ZFS for persistence";

      pool = mkOption {
        type = str;
        description = "The name of the ZFS pool to use";
      };
    };

    paths = {
      system = mkOption {
        type = str;
        default = "/persistent-system";
        description = "Path where system files are persisted";
      };
      homes = mkOption {
        type = str;
        default = "/persistent-homes";
        description = "Path where user homes are persisted";
      };
    };

    files = mkOption {
      type = listOf str;
      default = [ ];
      description = "List of system files to persist";
    };

    directories = mkOption {
      type = listOf str;
      default = [ ];
      description = "List of system directories to persist";
    };

    users = mkOption {
      default = { };
      description = "description of users that will have persistence enabled";
      type = attrsOf (submodule ({ name, ... }: {
        options = {
          name = mkOption {
            type = str;
            default = name;
            description = "username being configured";
          };
          files = mkOption {
            type = listOf str;
            default = [ ];
            description = "List of user files to persist";
          };
          directories = mkOption {
            type = listOf str;
            default = [ ];
            description = "List of user directories to persist";
          };
          xdg = {
            caches = mkOption {
              type = listOf str;
              default = [ ];
              description = "Caches to persist from XDG hierarchy";
            };
            configs = mkOption {
              type = listOf str;
              default = [ ];
              description = "Configurations to persist from XDG hierarchy";
            };
            data = mkOption {
              type = listOf str;
              default = [ ];
              description = "Data to persist from XDG hierarchy";
            };
            states = mkOption {
              type = listOf str;
              default = [ ];
              description = "States to persist from XDG hierarchy";
            };
          };
        };
      }));
    };
  };

  config = lib.mkMerge [
    # Generic support: setting paths, enable fuse, etc...
    (lib.mkIf cfg.enable {

      ##########
      # SYSTEM #
      ##########

      environment.persistence.${cfg.paths.system} = {
        inherit (cfg) directories;
        files = cfg.files ++ [ "/etc/machine-id" ];
      };

      environment.persistence.${cfg.paths.homes} = {
        hideMounts = true;
        users = lib.pipe cfg.users [
          lib.attrValues
          (map perUser)
          myLib.mergeAll
        ];
      };
    })
    # ZFS based setup:
    # - blank snapshot after creation
    # - auto restore blank snapshot at boot
    # - Create datasets for system, homes and the nix store
    (lib.mkIf (cfg.enable && cfg.zfs.enable) {
      boot = {
        loader.grub.zfsSupport = true;
        supportedFilesystems = [ "zfs" ];
        initrd.postDeviceCommands = lib.mkAfter (
          myLib.mkIfElse (!cfg.pause)
            "zfs rollback -r ${blankSnapshot cfg.zfs.pool} && echo 'Blank snapshot restored'"
            ""
        );
      };

      fileSystems = {
        ${cfg.paths.system}.neededForBoot = true;
        ${cfg.paths.homes}.neededForBoot = true;
      };

      disko.devices.zpool."${cfg.zfs.pool}" = {
        datasets = {
          root = zfs_fs "/" {
            postCreateHook = "zfs snapshot ${blankSnapshot cfg.zfs.pool}";
          };
          nix = zfs_fs "/nix" { };
          persistentSystem = zfs_fs cfg.paths.system { };
          persistentHomes = zfs_fs cfg.paths.homes { };
        };
      };
    })
  ];
}
