{
  inputs = {
    # Note: change to upstream later
    # common.url = "github:nammayatri/common";
    common.url = "github:arjunkathuria/common/Mobility-GHC927-Rebased";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    systems.url = "github:nix-systems/default";

    # Note: change to upstream later
    # shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel.url = "github:arjunkathuria/shared-kernel/GHC-927-testing-snapshot";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/arion-configuration.nix
        ./nix/docker.nix
      ];

      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.shared-kernel.haskellFlakeProjectModules.output
          ];
          devShell.tools = _: {
            inherit (self'.packages) arion;
          };
          autoWire = [ "packages" "checks" "apps" ];
        };

        process-compose.configs.run.processes = {
          beckn-gateway.command = lib.getExe self'.packages.beckn-gateway;
          mock-registry.command = lib.getExe self'.packages.mock-registry;
        };

        packages.default = self'.packages.beckn-gateway;

        devShells.default = pkgs.mkShell {
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
          ];
        };
      };
    };
}
