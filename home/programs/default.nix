args@{ config, pkgs-unstable, rootPath, ... }:
let
  more = { ... }: {
    xdg.configFile."doom".source =
      config.lib.file.mkOutOfStoreSymlink "${rootPath}/programs/doom";
    xdg.configFile."zed".source =
      config.lib.file.mkOutOfStoreSymlink "${rootPath}/programs/zed";

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

in [
  ./alacritty
  ./feh
  ./fzf
  ./git
  ./rofi
  (import ./vscode args)
  ./xmobar
  ./xmonad
  ./zsh
  more
]
