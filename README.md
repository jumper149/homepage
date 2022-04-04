# homepage

Felix Springer's Homepage hosted on [felixspringer.xyz](https://felixspringer.xyz/homepage/).

## Install

Install with a NixOS system flake and enable the service.

```nix
{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    homepage = {
      type = "github";
      owner = "jumper149";
      repo = "homepage";
      ref = "main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, homepage }@inputs: {
    nixosConfiguration = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [
        ({ config, pkgs, lib, inputs, ... }: {
          imports = [
            inputs.homepage.nixosModule
          ];
          services.homepage = {
            enable = true;
            config = {
              port = 8008;
              base-url = {
                scheme = "https";
                authority = {
                  host = "example.com";
                  port = null;
                };
                path = [];
              };
            };
          };
          services.nginx = {
            enable = true;
            virtualHosts."example.com" = {
              onlySSL = true;
              enableACME = true;
              locations."/" = {
                proxyPass = "http://127.0.0.1:${toString config.services.homepage.config.port}/";
              };
            };
          };
        })
        # ...
      ];
    };
  };
}
```

## Development

Enter a development shell.
Development tools are configured in `flake.nix`.

```
# Set up development environment with nix flakes.
nix develop

# Generate static files and configuration.
nix build .#defaultConfigFile.x86_64-linux

# Run to debug.
HOMEPAGE_CONFIG_FILE=result cabal run homepage
```

### Linting

Linting is configured in `flake.nix`.

```
# Test and lint.
nix flake check
```

### `weeder`

Running `weeder` should be a part of linting, but requires manual execution at the moment.

```
# Enter development shell.
nix develop

# Prepare additional information for weeder.
cabal clean
cabal build all

# Run weeder.
weeder
```
