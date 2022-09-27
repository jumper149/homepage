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
      blog = import ./blog/subflake.nix { inherit nixpkgs setup; };
      files = import ./files/subflake.nix { inherit nixpkgs setup; };
      static = import ./static/subflake.nix { inherit nixpkgs setup; };
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

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      writeScriptBin "homepage-full" ''
        HOMEPAGE_CONFIG_FILE="${self.packages.x86_64-linux.config}" ${self.subflakes.server.packages.x86_64-linux.default}/bin/homepage
      '';

    packages.x86_64-linux.config =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      writeText "homepage.json" (builtins.toJSON self.config);

    config =
      builtins.fromJSON (builtins.readFile ./homepage.json) // {
        revision = if self ? rev then self.rev else null;
        directory-blog = "${self.subflakes.blog.packages.x86_64-linux.default}";
        directory-files = "${self.subflakes.files.packages.x86_64-linux.default}";
        directory-static = "${self.subflakes.static.packages.x86_64-linux.default}";
      };

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

    nixosModules.default = { config, lib, ... }:
      let
        cfg = config.services.homepage;
        homepageConfig = lib.recursiveUpdate self.config cfg.config;
      in {
        options = {
          services.homepage = {
            enable = lib.mkEnableOption "Felix Springer's Homepage.";
            config = lib.mkOption {
              default = { };
              type = with lib.types; attrsOf anything;
              description = ''
                Configuration, that will be merged with default options and serialized to JSON.
                `lib.recursiveUpdate` is used to merge these changes.
              '';
            };
          };
        };
        config = lib.mkIf cfg.enable {
          environment = {
            etc."homepage.json".text = builtins.toJSON homepageConfig;
          };
          systemd.services.homepage = {
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            description = "Homepage";
            environment = {
              HOMEPAGE_CONFIG_FILE = "/etc/homepage.json";
              HOMEPAGE_LOG_FILE = "/var/log/homepage/access.log";
              HOMEPAGE_LOG_LEVEL = "LevelInfo";
            };
            restartTriggers = [
              config.environment.etc."homepage.json".source
            ];
            serviceConfig = {
              DynamicUser = true;
              ExecStart = "${self.subflakes.server.packages.x86_64-linux.default}/bin/homepage";
              LogsDirectory = "homepage";
            };
          };
        };
      };

  };
}
