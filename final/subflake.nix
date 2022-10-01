{ nixpkgs, setup, server, config }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    writeScriptBin "homepage-full" ''
      HOMEPAGE_CONFIG_FILE="${config.packages.x86_64-linux.default}" ${server.packages.x86_64-linux.default}/bin/homepage
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
}
