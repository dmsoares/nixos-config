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

  vscode = import ./vscode args;
  zsh = import ./zsh
    (args // { vscodePath = "${vscode.programs.vscode.package}/bin/code"; });

in [
  ./alacritty
  ./feh
  ./fzf
  ./git
  ./neovim
  ./rofi
  ./xmobar
  ./xmonad
  more
  vscode
  zsh
]
