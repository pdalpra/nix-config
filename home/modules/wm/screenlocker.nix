{ pkgs, lib, ... }:
let
  xsecurelock =
    pkgs.xsecurelock.overrideAttrs (previous: {
      buildInputs = with pkgs;
        (lib.lists.subtractLists [ apacheHttpd pamtester ] previous.buildInputs)
        ++ [ makeWrapper ];

      postInstall = (previous.postInstall or "") + ''
        wrapProgram "$out/bin/xsecurelock" \
        --set XSECURELOCK_SAVER saver_blank \
        --set XSECURELOCK_AUTHPROTO authproto_pam \
        --set XSECURELOCK_NO_COMPOSITE 1 \
        --set XSECURELOCK_FONT "Berkeley Mono:size=20:style=Medium,Regular" \
        --set XSECURELOCK_SHOW_DATETIME 1 \
        --set XSECURELOCK_BLANK_DPMS_STATE suspend \
        --set XSECURELOCK_SHOW_HOSTNAME 0 \
        --set XSECURELOCK_SAVER_STOP_ON_BLANK 1 \
        --set XSECURELOCK_DATETIME_FORMAT "%d/%m/%Y - %H:%M:%S"
      '';
    });
in
{
  # Restarts xss-lock automatically
  home.activation = {
    restartXssLock = lib.hm.dag.entryAfter [ "writeBoundary" ]
      "$DRY_RUN_CMD ${pkgs.systemd}/bin/systemctl --user restart xss-lock.service || true";
  };

  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${xsecurelock}/bin/xsecurelock";
    xss-lock.extraOptions = [
      "-l"
      "-n"
      "${xsecurelock}/libexec/xsecurelock/dimmer"
    ];
  };
}
