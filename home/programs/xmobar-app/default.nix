{ mkDerivation, pkgs }: with pkgs;
mkDerivation {
  pname = "xmobar-app";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = with haskellPackages; [ base extra process xmobar ];
  executableHaskellDepends = with haskellPackages; [ base xmobar ];
  doHaddock = false;
  description = "My Modified xmobar app";
  license = "unknown";
  mainProgram = "xmobar-app";
}
