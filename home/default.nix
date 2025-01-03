args@{ lib, _config, pkgs, pkgs-unstable, pkgs-22_11, ... }:

let
  username = "decio";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

in {

  imports = lib.concatMap (x: import x args) [ ./services ./programs ];

  home = {
    inherit username homeDirectory;

    packages = with pkgs; [
      discord
      gimp
      google-chrome
      gscreenshot
      kazam
      insomnia
      mate.atril
      telegram-desktop
      spotify
      pkgs-unstable.emacs
      pkgs-unstable.obsidian
      vlc
      watchman
      pkgs-unstable.zed-editor

      # beam
      erlang_26
      elixir
      gleam
      rebar3

      # c/c++
      gcc

      # haskell
      pkgs-unstable.cabal2nix
      pkgs-unstable.nix-prefetch-git
      pkgs-unstable.cabal-install
      pkgs-unstable.ghc
      pkgs-unstable.haskellPackages.haskell-language-server
      pkgs-unstable.haskellPackages.fourmolu
      pkgs-unstable.haskellPackages.cabal-gild

      # js
      nodejs
      nodePackages.npm
      pkgs-unstable.deno
      pkgs-unstable.nodePackages.eas-cli
      pkgs-22_11.nodePackages.pnpm # pnpm v7

      # infra
      awscli2
      pkgs-unstable.terraform
      kubectl
      ssm-session-manager-plugin # aws ssm plugin

      # misc
      alsa-lib
      fd
      fzf
      gh
      jq
      gnumake
      inotify-tools
      libnotify
      peek
      ripgrep
      xorg.xwininfo
      xclip
      zlib

      # nix
      nil
      nixd
      nixfmt

      # postgres
      jetbrains.datagrip

      # python
      python3
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
    portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

  };

  programs = { home-manager.enable = true; };

}
