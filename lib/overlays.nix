{ nixpkgs, nixpkgs-unstable, nurpkgs }: system:

let
  config = { allowUnfree = true; };
  pkgs = import nixpkgs { inherit config system; };
  nurOverlay = _: _: {
    nur = import nurpkgs {
      inherit pkgs;
      nurpkgs = pkgs;
    };
  };
  unstableOverlay = _: _: {
    unstable = import nixpkgs-unstable { inherit config system; };
  };
  steamOverlay = _: prev: {
    steam = prev.steam.override {
      extraEnv = {
        MANGOHUD = true;
        MANGOHUD_CONFIG = prev.lib.strings.concatStringsSep "," [
          "horizontal"
          "battery"
          "cpu_mhz"
          "battery_watt"
          "battery_time"
          "cpu_power"
          "gpu_power"
          "cpu_temp"
          "gpu_temp"
          "device_battery=gamepad,mouse"
          "ram"
          "vram"
        ];
      };
      extraPkgs = pkgs: with pkgs; [
        gamescope
        mangohud
      ];
    };
  };
in
import nixpkgs {
  inherit config system;

  overlays = [
    nurOverlay
    unstableOverlay
    steamOverlay
  ];
}

