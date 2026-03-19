{ config, ... }:
let inherit (config.theme.colorscheme) colors;
in {
  programs.alacritty = {
    enable = true;
    settings = {
      env.TERM = "xterm-256color";
      font = {
        normal = { family = "FiraCode Nerd Font"; };
        size = 12.0;
      };
      window = {
        padding = {
          x = 5;
          y = 5;
        };
      };
      colors = {
        primary = {
          background = "0x${colors.black2}";
          foreground = "0x${colors.white}";
        };
        normal = {
          black = "0x${colors.gray1}";
          red = "0x${colors.red}";
          green = "0x${colors.green}";
          yellow = "0x${colors.peach}";
          blue = "0x${colors.blue}";
          magenta = "0x${colors.mauve}";
          cyan = "0x${colors.teal}";
          white = "0x${colors.white}";
        };
        bright = {
          black = "0x${colors.gray2}";
          red = "0x${colors.maroon}";
          green = "0x${colors.green}";
          yellow = "0x${colors.yellow}";
          blue = "0x${colors.sapphire}";
          magenta = "0x${colors.pink}";
          cyan = "0x${colors.sky}";
          white = "0x${colors.subtext1}";
        };
      };
      scrolling.multiplier = 5;
      selection.save_to_clipboard = true;
    };
  };
}
