{ lib, pkgs, ... }: {
  programs.waybar = with lib; {
    enable = true;
    package = pkgs.waybar;

    settings = [{
      layer = "top";
      position = "top";
      height = 24;
      spacing = 5;

      modules-left = [ "hyprland/workspaces" "group/usage" ];
      modules-center = [ "clock" ];
      modules-right = [ "audio" "battery" "backlight" "group/net" "tray" ];

      "group/usage" = {
        "orientation" = "horizontal";
        "modules" = [ "memory" "temperature" ];
      };

      "group/net" = {
        "orientation" = "horizontal";
        "modules" = [ "network" "network#2" ];
      };

      "hyprland/workspaces" = {
        "format" = "{icon}";
        "on-click" = "activate";
        "icon-size" = 50;
        "sort-by-number" = true;
      };

      "clock" = { "format" = "{:%d.%m.%Y | %H:%M}"; };

      "audio" = {
        format = "{icon} {volume}% {format_source}";
        format-bluetooth = "{volume}% {icon} {format_source}";
        format-bluetooth-muted = " {icon} {format_source}";
        format-muted = " {format_source}";
        format-source = " {volume}%";
        format-source-muted = "";
        format-icons = {
          headphone = "";
          hands-free = "";
          headset = "";
          phone = "";
          portable = "";
          car = "";
          default = [ "" "" "" ];
        };
        on-click = "sleep 0.1 && pavucontrol";
      };

      "battery" = {
        states = {
          warning = 30;
          critical = 15;
        };
        format = "{icon} {capacity}%";
        format-charging = "󰂄 {capacity}%";
        format-plugged = "󱘖 {capacity}%";
        format-icons = [ "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹" ];
        on-click = "";
        tooltip = false;
      };

      backlight = {
        interval = 2;
        format = "{icon}";
        "format-icons" = [ "" "" "" "" "" "" "" ];
        "on-scroll-up" = "brightnessctl set +5%";
        "on-scroll-down" = "brightnessctl set 5%-";
        "smooth-scrolling-threshold" = 1;
      };

      memory = {
        interval = 30;
        format = "  {used:0.1f}G";
      };

      temperature = { "format" = " {temperatureC}°C"; };

      network = {
        interval = 5;
        format-wifi = "直";
        format-ethernet = "";
        format-linked = "";
        format-disconnected = "睊";
        format-disabled = "睊";
        tooltip-format = " {ifname} via {gwaddr}";
        on-click = "${pkgs.networkmanagerapplet}/bin/nm-connection-editor";
      };

      "network#2" = {
        interval = 5;
        format-wifi = "{essid}";
        format-ethernet = "{ipaddr}/{cidr}";
        format-linked = "{ifname} (No IP)";
        format-disconnected = "Disconnected";
        format-disabled = "Disabled";
        format-alt = " {bandwidthUpBits} |  {bandwidthDownBits}";
        tooltip-format = " {ifname} via {gwaddr}";
      };

      "bluetooth" = {
        "format" = "udb80udcaf";
        "format-disabled" = "udb80udcb2";
        "format-connected" = "udb80udcb1";
        "tooltip-format" = "{controller_alias}	{controller_address}";
        "tooltip-format-connected" = ''
          {controller_alias}	{controller_address}

          {device_enumerate}'';
        "tooltip-format-enumerate-connected" =
          "{device_alias}	{device_address}";
      };

      "hyprland/language" = { "format" = "{short}"; };

      "tray" = {
        "icon-size" = 16;
        "spacing" = 16;
      };
    }];

    style = concatStrings [''
      @define-color foreground #eff0f1;
      @define-color foreground-inactive #7f8c8d;
      @define-color background #232629;
      @define-color background-alt #31363b;

      * {
          font-family: Mononoki Nerd Font;
          font-size: 17px;
          padding: 0;
          margin: 0;
      }

      #waybar {
          color: @foreground;
          background-color: @background;
      }

      #workspaces button {
          padding-left: 1em;
          padding-right: 1.3em;
      }

      #workspaces button.empty {
          color: @foreground-inactive;
      }

      #workspaces button.active {
          background-color: @background-alt;
          border-radius: 3px;
      }

      #wireplumber,
      #bat,
      #tray,
      #usage,
      #net {
          background-color: @background-alt;
          border-radius: 3px;
          padding-left: 0.5em;
          padding-right: 0.5em;
          margin-left: 0.3em;
      }

      #battery,
      #memory,
      #language,
      #network {
          margin-right: 0.8em;
      }
    ''];
  };
}
