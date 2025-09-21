# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../configuration.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams = [
    "acpi.ec_no_wakeup=1" # Fixes ACPI wakeup issues
    "amdgpu.dcdebugmask=0x10" # Fixes Wayland slowdowns/freezes
  ];

  hardware.tuxedo-drivers.enable = true;
  hardware.tuxedo-control-center.enable = true;

  networking.hostName = "tuxedo-laptop"; # Define your hostname.

  services.power-profiles-daemon.enable = false;

  services.xserver.dpi = 242;

  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
    QT_FONT_DPI = "242";
  };

  # boot.initrd.kernelModules = [ "amdgpu" ];

  # services.xserver.enable = true;
  # services.xserver.videoDrivers = [ "amdgpu" ];

  # hardware = {
  #   tuxedo-rs = {
  #     enable = true;
  #     tailor-gui.enable = true;
  #   };
  # };
}
