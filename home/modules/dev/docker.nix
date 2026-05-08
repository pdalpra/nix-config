{ lib, config, ... }:

lib.mkIf (!builtins.elem "headless" config.profile) {
  home.sessionVariables = {
    DOCKER_SCAN_SUGGEST = "false";
  };
}
