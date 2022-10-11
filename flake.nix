{
  description = "Felix Springer's Homepage";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }: {

    subflakes = rec {
      setup = import ./setup/subflake.nix { };
      server = import ./server/subflake.nix { self = { subflakes = { inherit setup; }; }; inherit nixpkgs; };
      blog = import ./blog/subflake.nix { self = { subflakes = { inherit setup; }; inherit (self) rev; }; inherit nixpkgs; };
      files = import ./files/subflake.nix { self = { subflakes = { inherit setup; }; }; inherit nixpkgs; };
      static = import ./static/subflake.nix { self = { subflakes = { inherit setup; }; }; inherit nixpkgs; };
      config = import ./config/subflake.nix { self = { subflakes = { inherit setup blog files static; }; inherit (self) rev; }; inherit nixpkgs; };
      final = import ./final/subflake.nix { self = { subflakes = { inherit setup server config; }; }; inherit nixpkgs; };
    };

    packages.x86_64-linux.default = self.subflakes.final.packages.x86_64-linux.default;

    overlays.default = self.subflakes.final.overlays.default;

    nixosModules.default = self.subflakes.final.nixosModules.default;

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      let
        additionalBuildInputs = [
          pkgs.rnix-lsp
        ];
        shells = [
          self.subflakes.server.devShells.x86_64-linux.default
          self.subflakes.blog.devShells.x86_64-linux.default
          self.subflakes.static.devShells.x86_64-linux.default
        ];
        fullBuildInputs = __concatMap (x: x.buildInputs) shells;
        fullNativeBuildInputs = __concatMap (x: x.nativeBuildInputs) shells;
        fullShellHook = __concatStringsSep "\n" (map (x: x.shellHook) shells);
      in stdenv.mkDerivation {
        name = "homepage-development"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        installPhase = "touch $out";
        buildInputs = fullBuildInputs;
        nativeBuildInputs = fullNativeBuildInputs;
        shellHook = fullShellHook;
      };

    checks.x86_64-linux.subflakes =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      pkgs.mkShell {
        packages =
          let
            hasChecks = name: value: __elem "checks" (__attrNames value) && __elem "x86_64-linux" (__attrNames value.checks);
            checkableSubflakes = lib.filterAttrs hasChecks self.subflakes;
            checksBySubflake = __mapAttrs (name: value: value.checks.x86_64-linux) checkableSubflakes;
            checks = __foldl' (a: b: a ++ b) [ ] (map __attrValues (__attrValues checksBySubflake));
          in checks;
      };

  };
}
