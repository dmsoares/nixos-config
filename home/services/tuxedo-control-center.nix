{ lib, pkgs, ... }: {
  systemd.user.services.tuxedo-control-center = {
    Unit = { Description = "Tuxedo Control Center"; };
    Service = {
      ProtectSystem = "full";
      ProtectHostname = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictRealtime = true;
      Group = "users";
      ExecStart = "${lib.getExe pkgs.tuxedo-control-center}";
      Restart = "on-failure";
      RestartSec = 3;
      # Do not restart the service if a --resync is required which is done via a 126 exit code
      RestartPreventExitStatus = 126;
      # Time to wait for the service to stop gracefully before forcefully terminating it
      TimeoutStopSec = 90;
    };
    Install = { WantedBy = [ "default.target" ]; };
  };

  # systemd.user.targets.default.wants = [ "emacs.service" ];
}
