{ ... }: {
  programs.tmux = {
    enable = true;
    historyLimit = 10000;
    mouse = true;
  };
}
