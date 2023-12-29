{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    plugins = with pkgs; [ rofi-calc rofi-emoji ];
    terminal = "${pkgs.alacritty}/bin/alacritty";
    theme = "gruvbox-dark-hard";
    extraConfig = {
      kb-remove-to-eol = "";
      kb-accept-entry = "Control+m,Return,KP_Enter";
      kb-row-up = "Up,Control+k";
      kb-row-down = "Down,Control+j";
    };
  };

  # for rofi-emoji to insert emojis directly
  home.packages = [ pkgs.xdotool ];
}
