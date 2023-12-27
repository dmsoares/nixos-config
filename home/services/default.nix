let
  more = {
    services = {
      gnome-keyring = {
        enable = true;
        components = [ "pkcs11" "secrets" "ssh" ];
      };
    };
  };
in
[
  ./gpg-agent
  ./picom
  ./polybar
  more
]
