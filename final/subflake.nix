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
      ${server.packages.x86_64-linux.default}/bin/homepage-test-application
    '';

  overlays.default = final: prev: {
    homepage-jumper149.exe = server.packages.x86_64-linux.default;
    homepage-jumper149.full = packages.x86_64-linux.default;
    homepage-jumper149.config.default = config.config;
  };

  nixosModules.default = { config, lib, pkgs, ... }:
    {
      options = {
        nixpkgs.overlays = [
          overlays.default
        ];
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
      config = lib.mkIf config.services.homepage.enable {
        environment = {
          etc."homepage.json".text = builtins.toJSON (
            lib.recursiveUpdate pkgs.homepage-jumper149.config.default config.services.homepage.config
          );
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
            ExecStart = "${pkgs.homepage-jumper149.exe}/bin/homepage";
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
