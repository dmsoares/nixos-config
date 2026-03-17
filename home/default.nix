{ config, pkgs, pkgs-unstable, inputs, ... }:

let
  username = "decio";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

  theme = {
    package = pkgs.colloid-gtk-theme;
    name = "Colloid-Dark";
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

in {
  imports = [
    inputs.dms.homeModules.dank-material-shell
    inputs.danksearch.homeModules.default
    ./hyprland
    ./services
    ./programs
  ];

  # Theme selection — change to "gruvbox-dark" to switch all themed components
  # theme.name = "gruvbox-dark";

  home = {
    inherit username homeDirectory;

    packages = with pkgs; [
      # apps
      anki
      antigravity
      pkgs-unstable.claude-code
      discord
      gimp
      godot
      google-chrome
      gscreenshot
      kazam
      insomnia
      mate.atril
      pkgs-unstable.telegram-desktop
      spotify
      emacs
      pkgs-unstable.obsidian
      vlc
      watchman
      pkgs-unstable.zed-editor
      file-roller
      zoom-us
      syncthing
      syncthingtray

      # beam
      erlang_27
      elixir
      pkgs-unstable.gleam
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
      pnpm

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
      neofetch
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

    font = {
      package = pkgs.geist-font;
      name = "Geist";
      size = 12;
    };

    gtk2 = {
      extraConfig = "gtk-application-prefer-dark-theme = true";
      configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    };

    gtk3 = {
      bookmarks = [
        "file://${config.home.homeDirectory}/Dev"
        "file://${config.home.homeDirectory}/Documents"
        "file://${config.home.homeDirectory}/Downloads"
        "file://${config.home.homeDirectory}/Music"
        "file://${config.home.homeDirectory}/Pictures"
        "file://${config.home.homeDirectory}/Videos"
      ];
      extraConfig.gtk-application-prefer-dark-theme = true;
    };

    gtk4.extraConfig.gtk-application-prefer-dark-theme = true;
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      gtk-theme = theme.name;
    };
  };
}
