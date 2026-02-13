{ pkgs, ... }: {
  imports = [ ./config ./programs ./services ];

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = with pkgs.hyprlandPlugins; [ hyprexpo ];
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
  };
}
