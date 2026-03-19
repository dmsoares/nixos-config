args@{ ... }:
let dms = import ./dank-material-shell.nix args;
in {
  imports = [ dms ];

  programs.kitty.enable = true;
}
