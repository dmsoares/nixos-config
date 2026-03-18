{ pkgs, ... }: {
  imports = [ ./config ./programs ./services ];

  wayland.windowManager.hyprland = {
    enable = true;
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
  };
}
