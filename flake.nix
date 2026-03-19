{
  description = "NixOS configuration: development machine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    tuxedo-nixos.url = "github:sund3RRR/tuxedo-nixos";
    hyprland-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dms = {
      url = "github:AvengeMedia/DankMaterialShell/stable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    danksearch = {
      url = "github:AvengeMedia/danksearch";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dms-plugin-registry = {
      url = "github:AvengeMedia/dms-plugin-registry";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, nixpkgs-unstable, home-manager, tuxedo-nixos, ... }@inputs:
    let
      system = "x86_64-linux";
      home = [
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            backupFileExtension = "hmbackup_01";

            users.decio = import ./home;

            extraSpecialArgs = {
              pkgs-unstable = import nixpkgs-unstable {
                system = system;
                config.allowUnfree = true;
                config.permittedInsecurePackages = [ "electron-25.9.0" ];
              };
              inputs = inputs;
            };
            sharedModules = [ ./home/theme ];
          };
        }
      ];
      dms-overlay = final: prev: {
        dgop = (import nixpkgs-unstable {
          system = system;
          config.allowUnfree = true;
        }).dgop;
      };
    in {
      nixosConfigurations = {
        hp-desktop = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            ./machines/hp-desktop/configuration.nix
            { nixpkgs.overlays = [ dms-overlay ]; }
          ] ++ home;
        };

        tuxedo-laptop = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            ./machines/tuxedo-laptop/configuration.nix
            tuxedo-nixos.nixosModules.default
            { nixpkgs.overlays = [ dms-overlay ]; }
          ] ++ home;
        };
      };

      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
    };
}
