{ inputs, ... }: {
  imports = [
    inputs.dms.homeModules.dank-material-shell
    inputs.danksearch.homeModules.default
    inputs.dms-plugin-registry.modules.default
  ];

  programs.dank-material-shell = {
    enable = true;

    plugins = {
      dankBatteryAlerts.enable = true;
      nixMonitor.enable = true;
      emojiLauncher.enable = true;
      calculator.enable = true;
    };
  };

  programs.dsearch.enable = true;
}
