{
  description = "NixOS configuration: development machine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-22_11.url = "github:nixos/nixpkgs/nixos-22.11";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  };

  outputs = inputs@{ nixpkgs, nixpkgs-unstable, nixpkgs-22_11, home-manager, nixos-hardware, ... }: {
    nixosConfigurations = {
      nixos = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";

        modules = [
          ./configuration.nix

          # tuxedo pulse 15 gen2 specific configuration
          # nixos-hardware.nixosModules.tuxedo-pulse-15-gen2

          # uncomment to apply overlays
          # (args: { nixpkgs.overlays = import ./overlays args; })

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-backup";

            home-manager.users.decio = import ./home;

            home-manager.extraSpecialArgs = {
              pkgs-unstable = import nixpkgs-unstable {
                system = system;
                config.allowUnfree = true;
                config.permittedInsecurePackages = [ "electron-25.9.0" ];
              };
              pkgs-22_11 = import nixpkgs-22_11 {
                system = system;
                config.allowUnfree = true;
              };
            };
          }
        ];
      };
    };

    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
  };
}
