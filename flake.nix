{
  description = "NixOS configuration: development machine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-22_11.url = "github:nixos/nixpkgs/nixos-22.11";
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  };

  outputs = inputs@{ nixpkgs, nixpkgs-unstable, nixpkgs-22_11, home-manager, ... }: {
    nixosConfigurations = {
      nixos = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";

        modules = [
          ./configuration.nix


          (args: { nixpkgs.overlays = import ./overlays args; })

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.decio = import ./home/home.nix;

            home-manager.extraSpecialArgs = {
              pkgs-unstable = import nixpkgs-unstable {
                system = system;
                config.allowUnfree = true;
              };
              pkgs-22_11 = import nixpkgs-22_11 {
                system = system;
                config.allowUnfree = true;
              };
              # here to appease gvolpe's config
              # should be removed when I get more comfortable with NixOS
              hidpi = false;
            };
          }
        ];
      };
    };

    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
  };
}
