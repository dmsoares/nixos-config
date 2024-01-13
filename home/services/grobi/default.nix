{ pkgs, ... }:
{
  services.grobi = {
    enable = true;
    rules = [
      {
        name = "docked";
        outputs_connected = [ "eDP-1" "HDMI-1" ];
        atomic = true;
        configure_row = [ "eDP-1" "HDMI-1" ];
        primary = "eDP-1";
        execute_after = [
          "${pkgs.xmonad-with-packages}/bin/xmonad --restart"
        ];
      }
      {
        name = "undocked";
        outputs_disconnected = [ "HDMI-1" ];
        configure_single = "eDP-1";
        primary = true;
        atomic = true;
        execute_after = [
          "${pkgs.xmonad-with-packages}/bin/xmonad --restart"
        ];
      }
      {
        name = "fallback";
        configure_single = "eDP-1";
      }
    ];
  };
}
