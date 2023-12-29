{ pkgs, ... }:

{
  programs.xmobar = {
    enable = true;

    package = pkgs.haskellPackages.xmobar-app;
  };
}
