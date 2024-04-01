{ config, pkgs, impermanence, ... }:

with pkgs.lib;

let
  cfg = config.system.impermanence;
  persistentHomePath = user: "${cfg.paths.homes}/${user}";
  enableHomeManager = user: {
    "${user.name}" = {
      imports = [ impermanence.nixosModules.home-manager.impermanence ];

      home.persistence."${user.name}" = {
        inherit (user) files directories;
        persistentStoragePath = persistentHomePath user.name;
        allowOther = true;
      };
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
  options.system.impermanence = with types; {
    enable = mkEnableOption "enable impermanence";

    zfs = {
      enable = mkEnableOption "use ZFS for persistence";

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
        };
      }));
    };
  };

  config = mkMerge [
    # Generic support: setting paths, enable fuse, etc...
    (mkIf cfg.enable {

      ##########
      # SYSTEM #
      ##########

      environment.persistence.${cfg.paths.system} = {
        inherit (cfg) files directories;
        hideMounts = true;
      };

      ################
      # HOME MANAGER #
      ################

      programs.fuse.userAllowOther = mkForce true;

      home-manager.users = pipe cfg.users [
        attrValues
        (map enableHomeManager)
        mergeAll
      ];

      system.activationScripts.persistent-dirs.text =
        let
          mkHomePersist = persistedUser:
            let
              user = config.users.users.${persistedUser};
              path = persistentHomePath user.name;
            in
            lib.optionalString user.createHome ''
              mkdir -p ${path}
              chown ${user.name}:${user.group} ${path}
              chmod ${user.homeMode} ${path}
            '';
        in
        pipe cfg.users [
          attrNames
          (map mkHomePersist)
          concatLines
        ];
    })
    # ZFS based setup:
    # - blank snapshot after creation
    # - auto restore blank snapshot at boot
    # - Create datasets for system, homes and the nix store
    (mkIf (cfg.enable && cfg.zfs.enable) {
      boot.initrd.postDeviceCommands = lib.mkAfter ''
        zfs rollback -r ${blankSnapshot cfg.zfs.pool} && echo "Blank snapshot restored"
      '';

      fileSystems = {
        ${cfg.paths.system}.neededForBoot = true;
        ${cfg.paths.homes}.neededForBoot = true;
      };

      disko.devices.zpool."${cfg.zfs.pool}" = {
        rootFsOptions.canmount = "off";
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
