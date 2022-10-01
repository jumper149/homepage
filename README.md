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

## Subflakes

The source code is split up into subflakes, which also have their own development environments and checks.

* [Setup:](./setup) Nix overlays and configuration
* [Server:](./server) HTTP server executable
* [Blog:](./blog) Blog articles and related content
* [Files:](./files) Downloadable files
* [Static:](./files) Static files directly visible from HTML
* [Config:](./config) Runtime configuration with static files
* [Final:](./final) Wrapper and NixOS module

## Development

You can enter a development shell.

```
nix develop
```

## Tests

Nix flake checks are used for tests.

```
nix flake check
```
