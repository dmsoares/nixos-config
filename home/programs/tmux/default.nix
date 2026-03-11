{ ... }: {
  programs.tmux = {
    enable = true;
    extraConfig = ''
      set -g history-limit 10000
      setw -g mouse on
    '';
  };
}
