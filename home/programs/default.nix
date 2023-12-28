let
  more = { pkgs, ... }: {
    programs = {
      bat.enable = true;

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      gpg.enable = true;

      htop = {
        enable = true;
        settings = {
          sort_direction = true;
          sort_key = "PERCENT_CPU";
        };
      };

      jq.enable = true;

      ssh.enable = true;
    };
  };
in
[
  ./alacritty
  ./rofi
  ./xmonad
  ./zsh
  ./fzf
  more
]
