# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, ... }:

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

  # Diagnostic: enable AMD Smart Trace Buffer to debug suspend hangs
  boot.kernelParams = [ "amd_pmc.enable_stb=1" ];

  # Blacklist ucsi_acpi to prevent USB-C controller timeout on resume
  boot.blacklistedKernelModules = [ "ucsi_acpi" ];

  hardware.tuxedo-drivers.enable = true;
  hardware.tuxedo-control-center.enable = true;

  # Fix broken tccd-sleep.service (upstream bug: quoted ExecStart is treated as path)
  systemd.services.tccd-sleep.serviceConfig = {
    ExecStart = lib.mkForce "${pkgs.systemd}/bin/systemctl stop tccd";
    ExecStop = lib.mkForce "${pkgs.systemd}/bin/systemctl start tccd";
  };

  programs.steam.enable = true;

  services.logind.settings.Login = {
    HandleLidSwitch = "suspend";
    HandleLidSwitchExternalPower = "lock";
    HandleLidSwitchDocked = "ignore";
  };

  # Prevent lid-close from waking a suspended laptop on battery.
  # On s2idle (no S3), the EC always wakes on lid state change and there is
  # no LID entry in /proc/acpi/wakeup to disable.  Work around it by
  # re-suspending immediately when the system resumes with the lid closed
  # and no AC power (i.e. inside a bag).
  powerManagement.resumeCommands = ''
    lid=$(cat /proc/acpi/button/lid/LID1/state | ${pkgs.gawk}/bin/awk '{print $2}')
    ac=$(cat /sys/class/power_supply/AC0/online)
    if [ "$lid" = "closed" ] && [ "$ac" = "0" ]; then
      # brief settle time, then re-suspend
      sleep 2
      systemctl suspend
    fi
  '';

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
