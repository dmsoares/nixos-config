{ ... }:
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
  ./dunst
  ./gpg-agent
  ./picom
  ./trayer
  more
]
