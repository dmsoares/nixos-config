{
  services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 1.0;
    backend = "glx";
    vSync = true;
    fade = true;
    fadeDelta = 2;
    # shadow = true;
    # shadowOpacity = 0.75;
    # solving google chrome weird shadows around menus
    # https://github.com/orgs/regolith-linux/discussions/949
    wintypes = {
      tooltip = {
        fade = true;
        shadow = true;
        opacity = 0.75;
        focus = true;
        full-shadow = false;
      };
      dock = {
        shadow = false;
        clip-shadow-above = true;
      };
      dnd = { shadow = false; };
      menu = { shadow = false; };
      dropdown_menu = { opacity = 0.8; };
    };
  };
}
