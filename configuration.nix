# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:

{
  # Hosts file
  environment.etc.hosts.mode = "0644";
  # networking.extraHosts =
  #   ''
  #     127.0.0.1 ctt.dev.codeforall.io
  #   '';

  # networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Having trouble connecting to a public wifi network?
  # Temporarily uncomment this line:
  # networking.resolvconf.dnsExtensionMechanism = false;

  # Set your time zone.
  time.timeZone = "Europe/Lisbon";
  # time.timeZone = "America/Mexico_City";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pt_PT.UTF-8";
    LC_IDENTIFICATION = "pt_PT.UTF-8";
    LC_MEASUREMENT = "pt_PT.UTF-8";
    LC_MONETARY = "pt_PT.UTF-8";
    LC_NAME = "pt_PT.UTF-8";
    LC_NUMERIC = "pt_PT.UTF-8";
    LC_PAPER = "pt_PT.UTF-8";
    LC_TELEPHONE = "pt_PT.UTF-8";
    LC_TIME = "pt_PT.UTF-8";
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    settings = { General = { Enable = "Source,Sink,Media,Socket"; }; };
  };
  services.pulseaudio.enable = false;
  services.blueman.enable = true;

  systemd.services.upower.enable = true;

  users.groups.plugdev = { };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.decio = {
    isNormalUser = true;
    description = "Decio Soares";
    extraGroups = [ "networkmanager" "wheel" "docker" "plugdev" "adbusers" ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Enable Flakes and the new command-line tool
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    alacritty
    brightnessctl
    curl
    firefox
    gdb
    git
    gzip
    wget
    zip
    unzip
  ];

  # Set default editor to neovim
  environment.variables = {
    EDITOR = "nvim";
    TERMINAL = "alacritty";
  };

  # Aliases
  environment.interactiveShellInit = ''
    alias vim='nvim'
    alias suspend='systemctl suspend'
  '';

  # Home Manager interop
  environment.pathsToLink =
    [ "/share/applications" "/share/xdg-desktop-portal" ];

  # ZSH
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Thunar
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [ thunar-archive-plugin thunar-volman ];
  };
  programs.xfconf.enable = true;

  # Fonts
  fonts.packages = with pkgs; [
    meslo-lgs-nf
    monaspace
    nerd-fonts.hasklug
    nerd-fonts.fira-code
    nerd-fonts.droid-sans-mono
    nerd-fonts.symbols-only
  ];
  fonts.fontDir.enable = true;

  # Docker
  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = [ "decio" ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # android dev
  programs.adb.enable = true;

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    # Add any missing dynamic libraries for unpackaged programs
    # here, NOT in environment.systemPackages
    android-tools

    # electron dependencies
    glibc
    glib
    gtk3
    atk
    at-spi2-atk
    dbus
    libdrm
    pango
    cairo
    xorg.libX11
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXrandr
    xorg.libxcb
    xorg.libxshmfence
    libxkbcommon
    expat
    mesa
    alsa-lib
    at-spi2-core
    cups
    nss
    nspr
    ffmpeg
    gdk-pixbuf

    # haskell
    zlib
  ];

  # hyprland
  programs.hyprland.enable = true;
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  services = {
    gnome.gnome-keyring.enable = true;
    autorandr.enable = true;

    upower = {
      enable = true;
      criticalPowerAction = "Hibernate";
      percentageCritical = 5;
    };

    # Thunar
    gvfs.enable = true; # Mount, trash, and other functionalities
    tumbler.enable = true; # Thumbnail support for images

    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    displayManager = {
      gdm.enable = true;
      defaultSession = "hyprland";
    };

    xserver = {
      enable = true;

      xkb.layout = "us";
      xkb.variant = "altgr-intl";
      xkb.options = "caps:swapescape";

      autoRepeatDelay = 125;
      autoRepeatInterval = 20;

      desktopManager = { xterm.enable = false; };

      xautolock = {
        enable = true;
        time = 15;
        locker = "/run/wrappers/bin/slock";
        nowlocker = "/run/wrappers/bin/slock";
      };
    };

    libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
      touchpad.naturalScrolling = true;
      touchpad.tapping = true;
      touchpad.tappingDragLock = false;
      touchpad.middleEmulation = true;
      touchpad.accelSpeed = "0.5";
    };

    udev.extraRules = ''
      # Rules for Oryx web flashing and live training
      KERNEL=="hidraw*", ATTRS{idVendor}=="16c0", MODE="0664", GROUP="plugdev"
      KERNEL=="hidraw*", ATTRS{idVendor}=="3297", MODE="0664", GROUP="plugdev"

      # Legacy rules for live training over webusb (Not needed for firmware v21+)
        # Rule for all ZSA keyboards
        SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
        # Rule for the Moonlander
        SUBSYSTEM=="usb", ATTR{idVendor}=="3297", ATTR{idProduct}=="1969", GROUP="plugdev"
        # Rule for the Ergodox EZ
        SUBSYSTEM=="usb", ATTR{idVendor}=="feed", ATTR{idProduct}=="1307", GROUP="plugdev"
        # Rule for the Planck EZ
        SUBSYSTEM=="usb", ATTR{idVendor}=="feed", ATTR{idProduct}=="6060", GROUP="plugdev"

      # Wally Flashing rules for the Ergodox EZ
      ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
      ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
      KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

      # Keymapp / Wally Flashing rules for the Moonlander and Planck EZ
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", MODE:="0666", SYMLINK+="stm32_dfu"
      # Keymapp Flashing rules for the Voyager
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="3297", MODE:="0666", SYMLINK+="ignition_dfu"
    '';
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql;
    ensureDatabases = [ "mydatabase" ];
    authentication = pkgs.lib.mkOverride 10 ''
      #type database DBuser origin-address auth-method
      local all      all                   trust
      # ipv4
      host  all      all    127.0.0.1/32   trust
      # ipv6
      host  all      all    ::1/128        trust
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE decio WITH LOGIN CREATEDB;
      GRANT ALL PRIVILEGES ON DATABASE mydatabase TO decio;
      ALTER DATABASE mydatabase OWNER TO decio;
    '';
  };

  programs.slock.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ 8000 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Limit the number of generations to keep
  # boot.loader.systemd-boot.configurationLimit = 10;
  # boot.loader.grub.configurationLimit = 10;

  # Perform garbage collection weekly to maintain low disk usage
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # Optimize storage
  # You can also manually optimize the store via:
  #    nix-store --optimise
  # Refer to the following link for more details:
  # https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-auto-optimise-store
  nix.settings.auto-optimise-store = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}
