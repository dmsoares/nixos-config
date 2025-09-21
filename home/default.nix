args'@{ lib, config, pkgs, pkgs-unstable, ... }:

let
  username = "decio";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  rootPath = "${config.home.homeDirectory}/nixos-config/home";

  theme = {
    name = "palenight";
    package = pkgs.palenight-theme;
  };

  iconTheme = {
    name = "Papirus-Dark";
    package = pkgs.papirus-icon-theme;
  };

  cursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
    size = 24;
  };

  args = args' // { inherit rootPath; };

in {
  imports = lib.concatMap (x: import x args) [ ./services ./programs ];

  home = {
    inherit username homeDirectory;

    packages = with pkgs; [
      # apps
      anki
      discord
      gimp
      godot
      google-chrome
      gscreenshot
      kazam
      insomnia
      mate.atril
      telegram-desktop
      spotify
      emacs
      pkgs-unstable.obsidian
      vlc
      watchman
      pkgs-unstable.zed-editor
      file-roller
      pkgs-unstable.windsurf
      zoom-us
      jetbrains.idea-community
      syncthing
      syncthingtray

      # beam
      erlang_27
      elixir
      pkgs-unstable.gleam
      rebar3

      # c/c++
      gcc

      # dbs
      jetbrains.datagrip
      sqlite

      # haskell
      pkgs-unstable.cabal2nix
      pkgs-unstable.nix-prefetch-git
      pkgs-unstable.cabal-install
      pkgs-unstable.ghc
      pkgs-unstable.haskellPackages.haskell-language-server
      pkgs-unstable.haskellPackages.fourmolu
      pkgs-unstable.haskellPackages.cabal-gild

      # jvm
      # jdk24
      jdk23

      # js
      nodejs_20
      pnpm
      pkgs-unstable.deno
      # pkgs-unstable.nodePackages.eas-cli

      # infra
      awscli2
      pkgs-unstable.terraform
      kubectl
      ssm-session-manager-plugin # aws ssm plugin

      # markdown
      mdl
      texliveMedium # Emacs org mode

      # misc
      alsa-lib
      fd
      fzf
      gh
      jq
      gnome-themes-extra
      gnumake
      graphviz
      haskellPackages.hoogle
      inotify-tools
      libnotify
      pandoc
      pavucontrol
      peek
      ripgrep
      shellcheck
      xorg.xwininfo
      xclip
      zlib

      # nix
      nil
      nixd
      nixfmt-classic

      # python
      python3
    ];

    sessionVariables = {
      BROWSER = "google-chrome-stable";
      EDITOR = "nvim";
      TERMINAL = "alacritty";
    };

    pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
      name = cursor.name;
      package = cursor.package;
      size = cursor.size;
    };

    stateVersion = "23.11";
  };

  xdg = {
    enable = true;
    inherit configHome;
    portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      config = { common = { default = [ "gtk" ]; }; };
    };
  };

  programs = { home-manager.enable = true; };

  gtk = {
    enable = true;

    iconTheme = {
      name = iconTheme.name;
      package = iconTheme.package;
    };
    cursorTheme = {
      name = cursor.name;
      package = cursor.package;
    };
    theme = {
      name = theme.name;
      package = theme.package;
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = "1"; };
    gtk4.extraConfig = { gtk-application-prefer-dark-theme = "1"; };
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      gtk-theme = theme.name;
    };
  };

}
