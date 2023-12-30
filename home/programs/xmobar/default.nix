{ pkgs, ... }:

{
  programs.xmobar = {
    enable = true;

    # package = pkgs.haskellPackages.xmobar-app;

    extraConfig = builtins.readFile ./xmobarrc;
  };
}
