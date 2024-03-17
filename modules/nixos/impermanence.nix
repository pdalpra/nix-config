{ config, lib, myLib, impermanence, ... }:

with lib;
with myLib;

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
in
{
  options.system.impermanence = with types; {
    enable = mkEnableOption "enable impermanence";

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

  config = mkIf cfg.enable {

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

    # Required for home-manager integration
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
  };
}
