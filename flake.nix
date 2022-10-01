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
      server = import ./server/subflake.nix { inherit nixpkgs setup; };
      blog = import ./blog/subflake.nix { inherit self nixpkgs setup; };
      files = import ./files/subflake.nix { inherit nixpkgs setup; };
      static = import ./static/subflake.nix { inherit nixpkgs setup; };
      config = import ./config/subflake.nix { inherit self nixpkgs setup blog files static; };
      final = import ./final/subflake.nix { inherit nixpkgs setup server config; };
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

    packages.x86_64-linux.default = self.subflakes.final.packages.x86_64-linux.default;

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      self.subflakes.server.devShells.x86_64-linux.default.overrideAttrs (oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [
          pkgs.asciidoctor
          pkgs.imagemagick
          pkgs.lessc
          pkgs.rnix-lsp
        ];
      });

    nixosModules.default = self.subflakes.final.nixosModules.default;

    packages.x86_64-linux.homepage-test-application =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      writeScriptBin "homepage-test-application-full" ''
        export HOMEPAGE_CONFIG_FILE="${self.subflakes.config.packages.x86_64-linux.default}"
        export HOMEPAGE_LOG_LEVEL=LevelWarn
        ${self.subflakes.server.packages.x86_64-linux.test-application}/bin/homepage-test-application
      '';

    checks.x86_64-linux.homepage-test-application =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      stdenv.mkDerivation {
        name = "homepage-test-application"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          INIT_LOG="$(homepage-test-application-full)"
          echo "$INIT_LOG"

          if [ -z "$INIT_LOG" ];
          then
            echo "Successfully checked the initialization log."
          else
            echo "Warnings/Errors/Unknowns detected in initialization log."
            echo "$INIT_LOG"
            exit 1
          fi
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
          self.packages.x86_64-linux.homepage-test-application
        ];
      };

  };
}
