{ nixpkgs, setup, server, config }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    writeScriptBin "homepage-full" ''
      HOMEPAGE_CONFIG_FILE="${config.packages.x86_64-linux.default}" ${server.packages.x86_64-linux.default}/bin/homepage
    '';

  packages.x86_64-linux.homepage-test-application =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    writeScriptBin "homepage-test-application-full" ''
      export HOMEPAGE_CONFIG_FILE="${config.packages.x86_64-linux.default}"
      export HOMEPAGE_LOG_LEVEL=LevelWarn
      ${server.packages.x86_64-linux.test-application}/bin/homepage-test-application
    '';

  nixosModules.default = let configSubflake = config; in { config, lib, ... }:
    let
      cfg = config.services.homepage;
      homepageConfig = lib.recursiveUpdate configSubflake.config cfg.config;
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
            ExecStart = "${server.packages.x86_64-linux.default}/bin/homepage";
            LogsDirectory = "homepage";
          };
        };
      };
    };

  checks.x86_64-linux.homepage-test-application =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
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
        packages.x86_64-linux.homepage-test-application
      ];
    };

}
