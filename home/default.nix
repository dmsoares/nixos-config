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
      telegram-desktop
      spotify
      google-chrome
      bruno
      gscreenshot
      pkgs-unstable.emacs
      pkgs-unstable.obsidian

      # c/c++
      gcc

      # haskell
      pkgs-unstable.cabal2nix
      pkgs-unstable.nix-prefetch-git
      pkgs-unstable.cabal-install
      pkgs-unstable.ghc
      pkgs-unstable.haskellPackages.haskell-language-server
      pkgs-unstable.haskellPackages.fourmolu

      # js
      nodejs
      nodePackages.npm
      pkgs-22_11.nodePackages.pnpm #pnpm v7

      # infra
      awscli2
      pkgs-unstable.terraform
      kubectl

      # misc
      alsa-lib
      fd
      fzf
      gh
      jq
      peek
      ripgrep
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
