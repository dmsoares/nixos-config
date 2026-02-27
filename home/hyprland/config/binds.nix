{ ... }: {
  wayland.windowManager.hyprland.settings = {
    bind = let
      browser = "google-chrome-stable";
      editor = "zeditor";
      fileManager = "thunar";
    in [
      # Compositor commands
      "SUPER_SHIFT, P, pseudo"
      "SUPER, S, togglesplit"
      "SUPER, F, togglefloating"
      "SUPER, Q, killactive"
      "SUPER, Space, fullscreen"
      "SUPER, C, centerwindow"
      "SUPER_SHIFT, P, pin"

      # Move focus
      "SUPER, up, layoutmsg, cycleprev loop"
      "SUPER, K, layoutmsg, cycleprev loop"
      "SUPER, down, layoutmsg, cyclenext loop"
      "SUPER, J, layoutmsg, cyclenext loop"

      "SUPER, M, layoutmsg, focusmaster master"

      # Move windows
      "SUPER_SHIFT, up, layoutmsg, swapprev loop"
      "SUPER_SHIFT, K, layoutmsg, swapprev loop"
      "SUPER_SHIFT, down, layoutmsg, swapnext loop"
      "SUPER_SHIFT, J, layoutmsg, swapnext loop"

      "SUPER_SHIFT, M, layoutmsg, swapwithmaster master"

      # Cycle through windows
      "SUPER, o, focusworkspaceoncurrentmonitor, previous"

      # Special workspaces
      "SUPER, grave, togglespecialworkspace, magic"
      "SUPER_SHIFT, grave, movetoworkspace, special:magic"

      # Cycle workspaces
      "SUPER, bracketleft, focusworkspaceoncurrentmonitor, m-1"
      "SUPER, bracketright,focusworkspaceoncurrentmonitor, m+1"

      # Cycle monitors
      "SUPER, Y, focusmonitor, +1"

      # Send focused workspace to left/right monitors
      "SUPER_SHIFT ALT, bracketleft, movecurrentworkspacetomonitor, l"
      "SUPER_SHIFT ALT, bracketright, movecurrentworkspacetomonitor, r"

      # Application Shortcuts
      "SUPER, Return, exec, alacritty"
      "SUPER, B, exec, ${browser}"
      "SUPER, U, exec, emacsclient -c -a ''"
      "SUPER_SHIFT, U, exec, ${editor}"
      "SUPER, N, exec, ${fileManager}"

      "CTRL_ALT, L, exec, pgrep hyprlock || hyprlock"

      # Screenshot
      ", Print, exec, grimblast --notify copysave area"
      "CTRL, Print, exec, grimblast --notify --cursor copysave output"
      "ALT, Print, exec, grimblast --notify --cursor copysave screen"
    ] ++ builtins.concatLists (builtins.genList (x:
      let ws = let c = (x + 1) / 10; in builtins.toString (x + 1 - (c * 10));
      in [
        "SUPER, ${ws}, focusworkspaceoncurrentmonitor, ${toString (x + 1)}"
        "SUPER_SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
        "ALT_SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}"
      ]) 10);

    binde = [
      # Resize windows
      "SUPER_CTRL, left, resizeactive, -20 0"
      "SUPER_CTRL, H, resizeactive, -20 0"
      "SUPER_CTRL, right, resizeactive,  20 0"
      "SUPER_CTRL, L, resizeactive,  20 0"
      "SUPER_CTRL, up, resizeactive,  0 -20"
      "SUPER_CTRL, K, resizeactive,  0 -20"
      "SUPER_CTRL, down, resizeactive,  0 20"
      "SUPER_CTRL, J, resizeactive,  0 20"

      # Move windows
      "SUPER_ALT, left, moveactive, -20 0"
      "SUPER_ALT, H, moveactive, -20 0"
      "SUPER_ALT, right, moveactive,  20 0"
      "SUPER_ALT, L, moveactive,  20 0"
      "SUPER_ALT, up, moveactive,  0 -20"
      "SUPER_ALT, K, moveactive,  0 -20"
      "SUPER_ALT, down, moveactive,  0 20"
      "SUPER_ALT, J, moveactive,  0 20"
    ];

    bindr = [
      # Launcher
      ''
        SUPER, P, exec, rofi -show combi -modes combi -combi-modes "window,drun" -show-icons
      ''
      "SUPER_SHIFT, P, exec, rofi -modi emoji -show emoji -emoji-mode copy"
    ];

    bindl = [
      # Toggle mute
      ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
      ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle # Kept original"

      # Toggle airplane mode
      ",XF86RFKill, exec, networkctl toggle-network"
    ];

    bindle = [
      # Audio control
      ",XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
      ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"

      # Brightness control
      ",XF86MonBrightnessUp, exec, brightnessctl set 5%+ # Modified from -e4 -n2"
      ",XF86MonBrightnessDown, exec, brightnessctl set 5%- # Modified from -e4 -n2"
    ];

    # Mouse bindings
    bindm = [
      "SUPER, mouse:272, movewindow"
      "SUPER, mouse:273, resizewindow"
      "SUPER ALT, mouse:272, resizewindow"
    ];
  };
}
