{ config, lib, ... }:

with lib;

let
  profile = config.profile;
in
{
  options.profile = with types; mkOption {
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
        assertion = profile != null;
        message = "Profile must be set";
      }
    ];
  };
}
