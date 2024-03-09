{ lib, config, persistence, impermanence, ... }:

let
  persistentHomePath = user: "${persistence.homes}/${user}";
in
{

  environment.persistence.${persistence.system} = {
    hideMounts = true;

    files = [
      "/etc/machine-id"
    ];
  };

  home-manager.users.pdalpra = {
    imports = [
      impermanence.nixosModules.home-manager.impermanence
    ];

    home.persistence.pdalpra = {
      persistentStoragePath = persistentHomePath "pdalpra";
      allowOther = true;

      directories = [
        "Code"
        "Desktop"
        "Documents"
        "Downloads"
        "Music"
        "Pictures"
        "Videos"
        ".ssh"
        ".local/share/atuin"
      ];

    };
  };

  system.activationScripts.persistent-dirs.text =
    let
      users = lib.attrValues config.users.users;
      mkHomePersist = user:
        let
          path = persistentHomePath user.name;
        in
        lib.optionalString user.createHome ''
          mkdir -p ${path}
          chown ${user.name}:${user.group} ${path}
          chmod ${user.homeMode} ${path}
        '';
    in
    lib.concatLines (map mkHomePersist users);

}
