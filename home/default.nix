{ lib, config, pkgs, pkgs-unstable, ... }:

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
  imports = [ ./hyprland ./services ./programs ];

  # Catppuccin v0.1.3
  theme = {
    colorscheme = rec {
      colors = {
        rosewater = "F5E0DC";
        flamingo = "F2CDCD";
        pink = "F5C2E7";
        mauve = "CBA6F7";
        red = "F38BA8";
        maroon = "EBA0AC";
        peach = "FAB387";
        yellow = "F9E2AF";
        green = "A6E3A1";
        teal = "94E2D5";
        sky = "89DCEB";
        sapphire = "74C7EC";
        blue = "89B4FA";
        lavender = "B4BEFE";
        black0 = "11111B"; # crust
        black1 = "181825"; # mantle
        black2 = "1E1E2E"; # base
        black3 = "313244"; # surface0
        black4 = "45475A"; # surface1
        gray0 = "585B70"; # surface2
        gray1 = "6C7086"; # overlay0
        gray2 = "7F849C"; # overlay1
        overlay2 = "9399B2"; # overlay2
        subtext0 = "A6ADC8"; # subtext0
        subtext1 = "BAC2DE"; # subtext1
        white = "CDD6F4"; # text
      };

      xcolors = lib.mapAttrsRecursive (_: color: "#${color}") colors;
    };

    wallpaper = ./wallpapers/nix-wallpaper.png;
  };

  home = {
    inherit username homeDirectory;

    packages = with pkgs; [
      # apps
      anki
      antigravity
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
      jetbrains.idea-oss
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
