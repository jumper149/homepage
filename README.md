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
            inputs.homepage.nixosModules.default
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
nix build .#config

# Run to debug.
HOMEPAGE_CONFIG_FILE=result cabal run homepage
```

### Formatting

Use `fourmolu` to format Haskell.

```
# Format Haskell.
fourmolu --cabal-default-extensions --mode inplace $(find source -name '*.hs')
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

### `graphmod`

The module dependency graph can be visualised using `graphmod`.

```
# Enter development shell.
nix develop

# Run graphmod.
nix build .#checks.x86_64-linux.graphmod

# View graph with xdot or your PDF viewer.
xdot result/graphmod.dot
zathura result/graphmod.pdf
```

### `calligraphy`

The function call graph can be visualised using `calligraphy`.

```
# Enter development shell.
nix develop

# Prepare additional information for calligraphy.
cabal clean
cabal build all

# Run calligraphy.
calligraphy --output-stdout | xdot -
```
