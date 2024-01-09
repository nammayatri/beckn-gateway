# beckn-gateway

See [nammayatri's backend README](https://github.com/nammayatri/nammayatri/tree/main/Backend#getting-started) for general instructions on Nix-based development.

## Running

To run this project locally,

```sh
# This will spin up two processes: beckn-gateway and mock-registry
nix run .#run
```

To run the docker-compose containers (that the above processes depend on):

```sh
nix run .#arion
```

## Development

To autoformat and run other pre-commit checks, run:


```sh
# NOTE: This must be run from `nix develop` shell
pre-commit run
```

You can force running pre-commit on all file as follows:

```sh
# NOTE: This must be run from `nix develop` shell
pre-commit run -a
```

