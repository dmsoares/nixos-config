{ config, pkgs, vscodePath, ... }:

{
  programs.zsh = {
    enable = true;

    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    initExtraFirst = ''
      DISABLE_MAGIC_FUNCTIONS="true"
    '';

    initExtra = ''
      export ANDROID_HOME=$HOME/Android/Sdk
      export PATH=$PATH:$ANDROID_HOME/emulator
      export PATH=$PATH:$ANDROID_HOME/platform-tools
      export PATH=$PATH:$XDG_CONFIG_HOME/emacs/bin
    '';

    shellAliases = {
      vscode = vscodePath;
      zed = "zeditor";
      ns = "nix-shell --command $SHELL";
    };

    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
      {
        name = "powerlevel10k-config";
        src = ./p10k-config;
        file = "p10k.zsh";
      }
    ];

    zplug = {
      enable = true;
      plugins = [{ name = "zsh-users/zsh-autosuggestions"; }];
    };

    history = {
      size = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "vi-mode" "z" "aws" "terraform" ];
      theme = "robbyrussell";
    };
  };
}
