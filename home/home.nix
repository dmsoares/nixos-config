args@{ lib, config, pkgs, pkgs-unstable, pkgs-22_11, ... }:

let
  username = "decio";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

in

{

  imports = lib.concatMap (x: import x args) [
    ./services
    ./programs
  ];

  home = {
    inherit username homeDirectory;

    packages = with pkgs; [
      discord
      spotify
      google-chrome
      pkgs-unstable.emacs

      # c/c++
      gcc

      # haskell
      cabal2nix
      nix-prefetch-git
      cabal-install

      # js
      nodejs
      nodePackages.npm
      pkgs-22_11.nodePackages.pnpm #pnpm v7

      awscli2
      terraform
      kubectl

      # misc
      fd
      fzf
      jq
      ripgrep
      alsa-lib
    ];

    sessionVariables = {
      BROWSER = "google-chrome-stable";
      EDITOR = "nvim";
      TERMINAL = "alacritty";
    };

    stateVersion = "23.11";
  };

  xdg = {
    enable = true;
    inherit configHome;
  };

  programs = {
    home-manager.enable = true;
  };

}
