{ pkgs, ... }: {
  programs.waybar = {
    enable = true;
    package = pkgs.waybar;

    systemd = {
      enable = true;
      target = "graphical-session.target";
    };

    settings = [{
      layer = "top";
      position = "top";
      height = 24;
      spacing = 5;

      modules-left = [ "battery" "cpu" "memory" "disk" "network" ];
      modules-center = [ "custom/logo" "hyprland/workspaces" ];
      modules-right = [ "wireplumber" "clock" ];

      "battery" = {
        interval = 50;
        format = "{icon}   {time}";
        format-charging = "  {capacity}%";
        format-icons = [ "" "" "" "" "" ];
        states = {
          warning = 30;
          critical = 15;
        };
      };

      "cpu" = {
        interval = 2;
        format = " {usage}%";
        on-click = "alacritty -e htop";
        tooltip = false;
      };

      "memory" = {
        interval = 2;
        format = " {used}M ({percentage}%)";
        on-click = "alacritty -e htop";
        tooltip = false;
      };

      "disk" = {
        interval = 60;
        format = " {free}";
        path = "/";
        tooltip = false;
      };

      "network" = {
        interval = 1;
        format = " {bandwidthDownBytes}  {bandwidthUpBytes}";
        tooltip-format = "{ifname} via {gwaddr}";
      };

      "custom/logo" = {
        format = "λ";
        tooltip = false;
      };

      "hyprland/workspaces" = { persistent-workspaces = { "*" = 9; }; };

      "wireplumber" = {
        format = "{volume}%";
        format-muted = "";
        on-click = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
      };

      "clock" = {
        interval = 1;
        format = "{:%b %d %Y - %H:%M:%S}";
        tooltip = false;
      };
    }];

    style = ''
      * {
        font-family: "DejaVu Sans Mono", "Font Awesome 6 Free";
        font-size: 10pt;
        border: none;
        border-radius: 0;
        min-height: 0;
        padding: 0 2px;
      }

      window#waybar {
        background-color: #282828;
        color: #ebdbb2;
      }

      #cpu {
        color: #b8bb26;
        padding-right: 5px;
      }

      #memory {
        color: #458588;
        padding-right: 5px;
      }

      #disk {
        color: #d79921;
        padding-right: 5px;
      }

      #network {
        color: #b16286;
      }

      #wireplumber {
        color: #83a598;
        padding-right: 5px;
      }

      #custom-logo {
        color: #b16286;
        padding-right: 5px;
      }

      #workspaces button {
        color: #ebdbb2;
      }

      #workspaces button.active {
        color: #b8bb26;
      }
    '';
  };
}
