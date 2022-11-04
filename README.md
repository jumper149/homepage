# homepage

Felix Springer's Homepage hosted on [felixspringer.xyz](https://felixspringer.xyz/homepage/).

## Flake

You can use regular flake commands such as `nix build`, `nix develop` or `nix flake check`.

### Install

Use a NixOS system flake to enable the service.

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
    };
  };

  outputs = { self, nixpkgs, homepage }@inputs: {
    nixosConfigurations.default = nixpkgs.lib.nixosSystem {
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
These are just regular nix files, but follow the naming conventions of flakes.

* [Setup:](./setup) Nix overlays and configuration
* [Server:](./server) HTTP server executable
* [Blog:](./blog) Blog articles and related content
* [Files:](./files) Downloadable files
* [Static:](./static) Static files directly visible from HTML
* [Config:](./config) Runtime configuration with static files
* [Final:](./final) Wrapper and NixOS module
