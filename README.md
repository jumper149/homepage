# homepage

Felix Springer's Homepage.

```
# Set up development environment with nix flakes.
nix develop

# Generate static files and configuration.
nix build .#config

# Run to debug.
HOMEPAGE_CONFIG_FILE=result cabal run homepage
```
