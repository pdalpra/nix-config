{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    whatsapp-for-linux
    unstable.slack
    zoom-us
  ];

  xdg.configFile."whatsapp-for-linux/settings.conf".text =
    ''
      [web]
      allow-permissions=true
      hw-accel=1
      min-font-size=16

      [general]
      notification-sounds=true
      close-to-tray=true

      [appearance]
      prefer-dark-theme=true
    '';
}
