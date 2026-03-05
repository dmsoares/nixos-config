{ pkgs, config, ... }:
let
  rofiTheme = import ./theme.nix {
    pkgs = pkgs;
    config = config;
  };

in {
  programs.rofi = {
    enable = true;
    plugins = with pkgs; [ rofi-calc rofi-emoji ];
    terminal = "${pkgs.alacritty}/bin/alacritty";
    theme = "${rofiTheme.theme}";
    extraConfig = {
      kb-remove-to-eol = "";
      kb-accept-entry = "Control+m,Return,KP_Enter";
      kb-row-up = "Up,Control+k,Control+p";
      kb-row-down = "Down,Control+j,Control+n";
    };
  };

  # for rofi-emoji to insert emojis directly
  home.packages = with pkgs; [
    xdotool
    nerd-fonts.iosevka
    nerd-fonts.fantasque-sans-mono
    nerd-fonts.symbols-only
  ];
}
