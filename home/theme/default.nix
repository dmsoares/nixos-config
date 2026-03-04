{ lib, config, ... }:
let
  inherit (lib) mkOption mapAttrsRecursive;
  inherit (lib.types) attrsOf enum path str;

  themes = {
    "catppuccin-mocha" = import ./themes/catppuccin-mocha.nix;
    "gruvbox-dark" = import ./themes/gruvbox-dark.nix;
  };

  selected = themes.${config.theme.name};
in
{
  options.theme = {
    name = mkOption {
      type = enum (builtins.attrNames themes);
      default = "catppuccin-mocha";
      description = "Name of the active color theme.";
    };

    colorscheme = mkOption {
      type = attrsOf (attrsOf str);
      description = "The colors used in the theming.";
    };

    wallpaper = mkOption {
      type = path;
      description = "Wallpaper image.";
    };
  };

  config.theme = {
    colorscheme = {
      colors = selected.colors;
      xcolors = mapAttrsRecursive (_: color: "#${color}") selected.colors;
    };
    wallpaper = selected.wallpaper;
  };
}
