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

    packages.x86_64-linux.homepage =
      with import nixpkgs { system = "x86_64-linux"; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskellPackages.callCabal2nix "homepage" src {};

    packages.x86_64-linux.blog =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "blog"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/blog;
        buildPhase = ''
          mkdir -p static
          asciidoctor myWayToCoreboot.adoc --backend html5 --doctype article --out-file static/myWayToCoreboot.html --safe-mode secure --no-header-footer
          asciidoctor myOwnImplementationOfIExpressions.adoc --backend html5 --doctype article --out-file static/myOwnImplementationOfIExpressions.html --safe-mode secure --no-header-footer
          asciidoctor aSmallShowcaseOfBlucontrol.adoc --backend html5 --doctype article --out-file static/aSmallShowcaseOfBlucontrol.html --safe-mode secure --no-header-footer
        '';
        installPhase = ''
          cp --recursive static $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          asciidoctor
        ];
      };

    packages.x86_64-linux.files =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "files"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/files;
        buildPhase = ''
        '';
        installPhase = ''
          cp --recursive . $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          asciidoctor
        ];
      };

    packages.x86_64-linux.static =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "static"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/static;
        buildPhase = ''
          convert favicon.xpm favicon.png
          rm favicon.xpm
        '';
        installPhase = ''
          mkdir -p $out
          cp --recursive . $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          imagemagick
        ];
      };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          asciidoctor
          blaze-from-html
          ghcid
          haskell-language-server
          hlint
          hnix
          implicit-hie
          rnix-lsp
          weeder
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.homepage
        ];
        withHoogle = true;
      };

    nixosModule = { config, lib, ... }:
      let
        cfg = config.services.homepage;
        homepageConfig = {
          configDirectoryBlog = "${self.packages.x86_64-linux.blog}";
          configDirectoryFiles = "${self.packages.x86_64-linux.files}";
          configDirectoryStatic = "${self.packages.x86_64-linux.static}";
          configPort = 8008;
          configBaseUrl = "localhost:8008";
          configBlogEntries = {
            unBlogEntries = {
              myWayToCoreboot = {
                blogContent = "myWayToCoreboot";
                blogTimestamp = "2019-04-04";
                blogTitle = "my Way to Coreboot";
              };
              myOwnImplementationOfIExpressions = {
                blogContent = "myOwnImplementationOfIExpressions";
                blogTimestamp = "2019-06-30";
                blogTitle = "my own Implementation of I-Expressions";
              };
              aSmallShowcaseOfBlucontrol = {
                blogContent = "aSmallShowcaseOfBlucontrol";
                blogTimestamp = "2021-09-27";
                blogTitle = "a small Showcase of blucontrol";
              };
            };
          };
        } // cfg.extraConfig;
      in {
        options = {
          services.homepage = {
            enable = lib.mkOption {
              default = false;
              type = with lib.types; bool;
              description = ''
                Felix Springer's Homepage.
              '';
            };
            extraConfig = lib.mkOption {
              default = { };
              type = with lib.types; attrsOf anything;
              description = ''
                Configuration, that will be merged with default options and serialized to JSON.
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
            };
            restartTriggers = [
              config.environment.etc."homepage.json".source
            ];
            serviceConfig = {
              DynamicUser = true;
              ExecStart = "${self.packages.x86_64-linux.homepage}/bin/homepage";
              LogsDirectory = "homepage";
            };
          };
        };
      };

  };
}
