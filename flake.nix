{
  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    haskell-flake.follows = "common/haskell-flake";

    shared-kernel.url = "github:nammayatri/shared-kernel/backend/feat/frfs-bus-integration";
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

        # FIX ME: confirm what the "process-compose" field needs to be
        process-compose = { };

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
