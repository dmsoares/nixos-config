{ lib, config, pkgs, pkgs-unstable, pkgs-22_11, ... }:

let
  username = "decio";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

in

{
  home = {
    inherit username homeDirectory;

    packages = with pkgs; [
      discord
      google-chrome

      # utils
      fd
      fzf
      jq
      ripgrep

      # dev
      nodejs
      nodePackages.npm
      pkgs-22_11.nodePackages.pnpm #pnpm v7

      libgcc

      cabal2nix
      nix-prefetch-git
      cabal-install

      # libs
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

  imports = lib.concatMap import [
    ./services
    ./programs
  ];

  programs = {
    home-manager.enable = true;

    git = {
      enable = true;
      userName = "Decio Soares";
      userEmail = "decio.msoares@gmail.com";
    };

    vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        esbenp.prettier-vscode
        dbaeumer.vscode-eslint
        bradlc.vscode-tailwindcss
        ms-vscode.cpptools
        github.copilot
        bbenoist.nix
        hashicorp.terraform
        yzhang.markdown-all-in-one
      ];
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
}
