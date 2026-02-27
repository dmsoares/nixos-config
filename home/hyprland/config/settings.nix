{ config, ... }: {
  wayland.windowManager.hyprland.settings = let
    inherit (config.theme.colorscheme) colors;
    pointer = config.home.pointerCursor;
  in {
    monitor = [
      # name, resolution, position, scale
      "eDP-1, preferred, auto, 1.5"
      "HDMI-A-1, highres, auto-up, auto"
    ];

    env = [
      "GDK_SCALE,2"
      "XCURSOR_SIZE,32"
      "CLUTTER_BACKEND,wayland"
      "GDK_BACKEND,wayland,x11,*"
      "SDL_VIDEODRIVER,wayland"
      "XDG_CURRENT_DESKTOP,Hyprland"
      "XDG_SESSION_DESKTOP,Hyprland"
      "XDG_SESSION_TYPE,wayland"
      "QT_AUTO_SCREEN_SCALE_FACTOR,1"
      "QT_QPA_PLATFORM,wayland;xcb"
      "QT_QPA_PLATFORMTHEME,qt5ct"
      "QT_STYLE_OVERRIDE,kvantum"
      "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
      "GTK_THEME,${config.gtk.theme.name}"
      "XCURSOR_THEME,${pointer.name}"
      "XCURSOR_SIZE,${toString pointer.size}"
    ];

    exec-once = [
      "hyprpanel"
      "hyprctl setcursor ${pointer.name} ${toString pointer.size}"
    ];

    general = {
      gaps_in = 0;
      gaps_out = 0;

      border_size = 2;
      "col.active_border" = "rgb(${colors.blue})";
      "col.inactive_border" = "rgb(${colors.gray0})";

      resize_on_border = true;

      # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
      allow_tearing = false;

      layout = "master";
    };

    dwindle = {
      pseudotile = true;
      preserve_split = true;
    };

    master = { new_status = "master"; };

    decoration = {
      rounding = 0;

      active_opacity = 1.0;
      inactive_opacity = 1.0;
      fullscreen_opacity = 1.0;

      dim_inactive = false;

      blur.enabled = false;
      shadow.enabled = false;
    };

    animations = {
      enabled = false;

      bezier = [ "easeOutQuart, 0.25, 1, 0.5, 1" ];

      animation = [
        "windows, 1, 3, easeOutQuart, slide"
        "layers, 1, 3, easeOutQuart, fade"
        "fade, 1, 3, easeOutQuart"
        "border, 1, 5, easeOutQuart"
        "workspaces, 1, 5, easeOutQuart, slide"
        "specialWorkspace, 1, 5, easeOutQuart, slidevert"
      ];
    };

    input = {
      kb_layout = "us";
      kb_variant = "altgr-intl";
      kb_options = "caps:swapescape";
      repeat_delay = 125;
      repeat_rate = 30;

      follow_mouse = 1;
      accel_profile = "flat";
      sensitivity = 0.5; # -1.0 - 1.0, 0 means no modification.

      touchpad = {
        disable_while_typing = true;
        natural_scroll = true;
        tap-to-click = true;
        tap-and-drag = true;
        scroll_factor = 0.5;
      };
    };

    misc = {
      animate_manual_resizes = true;
      animate_mouse_windowdragging = true;
      disable_autoreload = true;
      disable_hyprland_logo = true;
      force_default_wallpaper = 0;
      vfr = true;
      vrr = 1;
    };

    binds = { allow_workspace_cycles = true; };

    xwayland = {
      enabled = true;
      force_zero_scaling = true;
    };

    gesture = [
      "3, horizontal, workspace"
      "3, up, dispatcher, hyprexpo:expo"
      "3, down, close"
      "4, swipe, move"
    ];

    plugin = {
      hyprexpo = {
        columns = 3;
        gap_size = 5;
        bg_col = "rgb(${colors.black0})";
        workspace_method =
          "center current"; # [center/first] [workspace] e.g. first 1 or center m+1
        gesture_distance = 300; # how far is the "max" for the gesture
      };
    };

    # plugin = {
    #   hyprexpo = {
    #     columns = 3;
    #     gap_size = 4;
    #     bg_col = "rgb(${colors.black0})";
    #     enable_gesture = true;
    #     gesture_fingers = 3;
    #     gesture_distance = 300;
    #     gesture_positive = false;
    #   };
    # };
  };
}
