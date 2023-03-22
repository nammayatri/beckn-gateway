{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";

    shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.default
      ];

      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.shared-kernel.haskellFlakeProjectModules.output
          ];
        };

        packages.default = pkgs.linkFarmFromDrvs "packages-combined"
          (builtins.attrValues config.haskellProjects.default.outputs.localPackages);
      };
    };
}
