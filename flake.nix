{
  description = "NixOS configuration: development machine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, ... }: {
    nixosConfigurations = {
      nixos = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";

        modules = [
          ./configuration.nix

          # uncomment to apply overlays
          # (args: { nixpkgs.overlays = import ./overlays args; })

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hmbackup_00";

            home-manager.users.decio = import ./home;

            home-manager.extraSpecialArgs = {
              pkgs-unstable = import nixpkgs-unstable {
                system = system;
                config.allowUnfree = true;
                config.permittedInsecurePackages = [ "electron-25.9.0" ];
              };
            };
          }
        ];
      };
    };

    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
  };
}
