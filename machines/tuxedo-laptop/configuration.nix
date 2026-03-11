# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../configuration.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel 6.14+ required for Hawk Point APU (ACP 7.0 DMIC, etc.)
  boot.kernelPackages = pkgs.linuxPackages_latest;

  hardware.tuxedo-drivers.enable = true;
  hardware.tuxedo-control-center.enable = true;

  programs.steam.enable = true;

  services.logind.settings.Login = {
    HandleLidSwitch = "suspend";
    HandleLidSwitchExternalPower = "lock";
    HandleLidSwitchDocked = "ignore";
  };

  services.atd.enable = true;

  services.openssh.enable = true;

  services.tailscale.enable = true;
  environment.systemPackages = [ pkgs.tailscale ];

  # Firmware (includes AMD ACP/DMIC firmware needed for internal microphone)
  hardware.enableRedistributableFirmware = true;

  # Audio (PipeWire)
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  networking.hostName = "tuxedo-laptop"; # Define your hostname.
}
