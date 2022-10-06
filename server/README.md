# Server

The server executable is written in Haskell using wai and servant for the HTTP interface.
The executable expects environment variables and a configuration file.

## Configuration

Environment variables and a configuration file change the runtime behaviour.

### Environment

Environment variables are checked early during the initialization.
The options are set [here](./source/library/Homepage/Environment.hs).

### Configuration file

The configuration file is written in JSON and is specified in [here](./source/library/Homepage/Configuration.hs).

## Development

```
# Enter development shell.
nix develop ..#subflakes.server.devShells.x86_64-linux.default

# Generate static files and configuration.
nix build ..#config

# Run to debug.
HOMEPAGE_CONFIG_FILE=result cabal run homepage
```

### Formatting

Use `fourmolu` to format Haskell.

```
# Enter development shell.
nix develop ..#subflakes.server.devShells.x86_64-linux.default

# Format Haskell.
fourmolu --mode inplace ./source
```

### `weeder`

Running `weeder` should be a part of linting, but requires manual execution at the moment.

```
# Enter development shell.
nix develop ..#subflakes.server.devShells.x86_64-linux.default

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
nix develop ..#subflakes.server.devShells.x86_64-linux.default

# Run graphmod.
nix build ..#subflakes.server.checks.x86_64-linux.graphmod

# View graph with xdot or your PDF viewer.
xdot result/graphmod.dot
zathura result/graphmod.pdf
```

### `calligraphy`

The function call graph can be visualised using `calligraphy`.

```
# Enter development shell.
nix develop ..#subflakes.server.devShells.x86_64-linux.default

# Prepare additional information for calligraphy.
cabal clean
cabal build all

# Run calligraphy.
calligraphy --output-stdout | xdot -
```
