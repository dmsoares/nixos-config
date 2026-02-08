{ pkgs, ... }: {
  imports = [
    ./config/binds.nix
    ./config/rules.nix
    ./config/settings.nix
    ./programs/waybar
    ./programs/wlogout
    ./services/cliphist.nix
    ./services/dunst.nix
    ./services/hypridle.nix
    ./services/hyprlock.nix
    ./services/hyprpaper.nix
    ./services/polkit-agent.nix
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = with pkgs.hyprlandPlugins; [ hyprexpo ];
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
  };

  programs.kitty.enable = true;
}
