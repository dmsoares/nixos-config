{ config, ... }:
let rootPath = "${config.home.homeDirectory}/nixos-config/home";
in {
  imports = [ ./alacritty ./fzf ./git ./neovim ./rofi ./zsh ];

  xdg.configFile."doom".source =
    config.lib.file.mkOutOfStoreSymlink "${rootPath}/programs/doom";
  xdg.configFile."zed".source =
    config.lib.file.mkOutOfStoreSymlink "${rootPath}/programs/zed";

  programs = {
    bat.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    gpg.enable = true;

    htop = {
      enable = true;
      settings = {
        sort_direction = true;
        sort_key = "PERCENT_CPU";
      };
    };

    btop.enable = true;

    jq.enable = true;

    ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks."*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
    };
  };
}
