{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";

    shared-kernel.url = "github:nammayatri/shared-kernel/nixify";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.default
      ];

      perSystem = { self', pkgs, lib, config, ... }: {
        cachix-push.packages = [ "all" ];

        haskellProjects.default = {
          imports = [
            inputs.shared-kernel.haskellFlakeProjectModules.output
          ];
          basePackages = config.haskellProjects.ghc810.outputs.finalPackages;
          packages = {
            beckn-gateway.root = ./app/gateway;
            mock-registry.root = ./app/mock-registry;
          };
        };

        packages.default = self'.packages.beckn-gateway;

        # A dummy package to force build of all local Haskell packages. 
        # Useful in CI.
        packages.all = pkgs.runCommand "packages-combined"
          {
            all = builtins.attrValues config.haskellProjects.default.outputs.localPackages;
          } '' echo $all > $out '';
      };
    });
}
