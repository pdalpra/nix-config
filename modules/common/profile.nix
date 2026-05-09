{ config, lib, ... }:

{
  options.profile = with lib; with types; mkOption {
    description = "Usage profile tags for this machine. Must be non-empty.";
    default = [ ];
    type = listOf (enum [
      "personal"
      "work"
      "headless"
    ]);
  };

  config = {
    assertions = [
      {
        assertion = config.profile != [ ];
        message = "Profile must be non-empty";
      }
    ];
  };
}
