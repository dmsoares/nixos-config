{ lib, ... }:

{
  programs.alacritty = {
    enable = true;
    # custom settings
    settings = {
      env.TERM = "xterm-256color";
      font = {
        size = 10;
      };
      colors = import ./gruvbox-dark.nix;
      scrolling.multiplier = 5;
      selection.save_to_clipboard = true;
    };
  };
}
