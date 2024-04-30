_:

{
  system.impermanence = {
    enable = true;

    files = [
      "/etc/machine-id"
    ];

    users.pdalpra = {
      directories = [
        "Code"
        "Desktop"
        "Documents"
        "Downloads"
        "Music"
        "Pictures"
        "Videos"
        ".ssh"
      ];

      xdg = {
        data = [
          "atuin"
        ];
      };
    };
  };
}
