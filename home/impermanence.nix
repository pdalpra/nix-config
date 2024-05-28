{

  files = [
    ".zsh_history"
  ];

  directories = [
    "Code"
    "Desktop"
    "Documents"
    "Downloads"
    "Music"
    "Pictures"
    "Videos"
    ".ssh"
    ".gnupg"
    ".steam"
    ".mozilla"
    ".ivy2"
    ".rustup"
    ".thunderbird"
    ".vscode"
  ];

  xdg = {
    caches = [
      "nix"
      "nix-index"
      "JetBrains"
      "coursier"
      "cabal"
      "ghcide"
      "hie-bios"
      "chromium"
      "mozilla"
      "tealdeer"
    ];

    configs = [
      "chromium"
      "Slack"
      "Code"
      "light"
      "1Password"
      "protonmail"
      "JetBrains"
      "gh"
    ];

    data = [
      "direnv"
      "keyrings"
      "zoxide"
      "protonmail"
      "JetBrains"
      "Steam"
      "whatsapp-for-linux"
    ];

    states = [
      "cabal"
    ];
  };
}
