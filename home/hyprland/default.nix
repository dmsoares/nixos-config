args@{ ... }:
let programs = import ./programs args;
in {
  imports = [ ./config programs ];

  wayland.windowManager.hyprland = {
    enable = true;
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
  };
}
