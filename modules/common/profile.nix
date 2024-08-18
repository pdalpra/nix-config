{ config, lib, ... }:

{
  options.profile = with lib; with types; mkOption {
    description = "Usage profile for this machine. Must be set.";
    default = null;
    type = nullOr (enum [
      "personal"
      "work"
    ]);
  };

  config = {
    assertions = [
      {
        assertion = config.profile != null;
        message = "Profile must be set";
      }
    ];
  };
}
