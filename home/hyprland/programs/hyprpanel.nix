{ pkgs, ... }: {
  programs.hyprpanel = {
    enable = true;
    settings = {
      bar = {
        layouts = {
          # See 'https://hyprpanel.com/configuration/panel.html'.
          "0" = {
            "left" = [ "dashboard" "workspaces" "windowtitle" ];
            "middle" = [ "media" "netstat" ];
            "right" = [
              "volume"
              "network"
              "bluetooth"
              "battery"
              "systray"
              "clock"
              "notifications"
            ];
          };
          "1" = {
            "left" = [ "dashboard" "workspaces" "windowtitle" ];
            "middle" = [ "media" ];
            "right" = [ "volume" "clock" "notifications" ];
          };
          "2" = {
            "left" = [ "dashboard" "workspaces" "windowtitle" ];
            "middle" = [ "media" ];
            "right" = [ "volume" "clock" "notifications" ];
          };
        };

        launcher = {
          # icon = "❄️";
          autoDetectIcon = true;
        };

        workspaces = {
          show_icons = false;
          show_numbered = true;
          numbered_active_indicator = "underline";
          showWsIcons = false;
          showApplicationIcons = false;
          workspaces = 9;
          ignored = "-98";
          monitorSpecific = false;
        };
      };

      menus = {
        transition = "crossfade";

        media = {
          displayTimeTooltip = false;
          displayTime = false;
        };

        volume.raiseMaximumVolume = true;

        clock = {
          weather.location = "Lisbon";
          time.military = false;
        };

        # dashboard.powermenu.avatar.image =
        #   toString config.home.file."Media/avatar.jpg".source;
      };

      scalingPriority = "hyprland";

      theme = {
        font = {
          name = "FiraCode Nerd Font Regular";
          size = "1rem";
          weight = "400";
        };

        notification.opacity = 100;

        osd.scaling = 90;

        bar = {
          menus.opacity = 100;
          transparent = false;
          border.location = "none";

          outer_spacing = "0";

          scaling = 80;

          menus.menu.dashboard.scaling = 90;
          menus.menu.media.scaling = 90;
          menus.menu.volume.scaling = 90;
          menus.menu.network.scaling = 90;
          menus.menu.bluetooth.scaling = 90;
          menus.menu.battery.scaling = 90;
          menus.menu.clock.scaling = 90;
          menus.menu.notifications.scaling = 90;

          buttons = {
            style = "default";
            monochrome = false;
            enableBorders = false;
            innerRadiusMultiplier = "0.4";
            radius = "0.4";

            dashboard.enableBorder = false;
            workspaces.enableBorder = false;
            windowtitle.enableBorder = false;
            modules.kbLayout.enableBorder = false;
          };
        };
      };
    };
  };

  home.packages = with pkgs; [
    # optional deps
    gpustat
    gpu-screen-recorder
    hyprpicker
    hyprsunset
    hypridle
    btop
    matugen
    swww
    grimblast
  ];
}
