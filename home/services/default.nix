{ ... }: {
  imports = [ ./gpg-agent ./tuxedo-control-center.nix ];

  services = {
    gnome-keyring = {
      enable = true;
      components = [ "pkcs11" "secrets" "ssh" ];
    };
  };
}
