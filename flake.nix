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

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      writeScript "homepage-full" ''
        HOMEPAGE_CONFIG_FILE="${self.packages.x86_64-linux.config}" ${self.packages.x86_64-linux.homepage}/bin/homepage
      '';

    packages.x86_64-linux.homepage =
      with import nixpkgs { system = "x86_64-linux"; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskellPackages.callCabal2nix "homepage" src {};

    packages.x86_64-linux.config =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        config = builtins.fromJSON (builtins.readFile ./homepage.json) // {
          directory-blog = "${self.packages.x86_64-linux.blog}";
          directory-files = "${self.packages.x86_64-linux.files}";
          directory-static = "${self.packages.x86_64-linux.static}";
        };
      in writeText "homepage.json" (builtins.toJSON config);

    packages.x86_64-linux.blog =
      with import nixpkgs { system = "x86_64-linux"; };
      let blogIds = builtins.attrNames (builtins.fromJSON (builtins.readFile ./homepage.json)).blog-entries;
      in stdenv.mkDerivation {
        name = "blog"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/blog;
        buildPhase = ''
          mkdir -p static

          ASCIIDOCTOR_FLAGS="--doctype article --safe-mode server --attribute nofooter --attribute source-highlighter=rouge"

          compileArticle() {
            ARTICLE_NAME="$1"

            if [ -f "$ARTICLE_NAME.adoc" ]
            then
              echo "Article exists: '$ARTICLE_NAME'"

              echo "HTML: '$ARTICLE_NAME'"
              asciidoctor "$ARTICLE_NAME.adoc" --backend html5 $ASCIIDOCTOR_FLAGS --out-file "static/$ARTICLE_NAME.html"
              sed -i 's/^<head>$/<head>\n<base target="_parent">/' "static/$ARTICLE_NAME.html"

              echo "PDF: '$ARTICLE_NAME'"
              asciidoctor-pdf "$ARTICLE_NAME.adoc" $ASCIIDOCTOR_FLAGS --out-file "static/$ARTICLE_NAME.pdf"

              if [ -d "$ARTICLE_NAME" ]
              then
                echo "Additional directory exists: '$ARTICLE_NAME'"
                cp -r $ARTICLE_NAME static
              else
                echo "Additional directory doesn't exist: '$ARTICLE_NAME'"
              fi
              else
                echo "Article doesn't exist: '$ARTICLE_NAME'"
                exit 2
            fi
          }

          for blogId in ${lib.strings.escapeShellArgs blogIds}
          do
            compileArticle "$blogId"
          done
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
          ln -s favicon.png favicon.ico

          convert icons/feed.png -resize 128x128 icons/feed.png
          convert icons/GitHub.png -resize 128x128 icons/GitHub.png
          convert icons/GitLab.png -resize 128x128 icons/GitLab.png
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
          pkgs.imagemagick
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
        homepageConfig = builtins.fromJSON (builtins.readFile self.packages.x86_64-linux.config) // {
          port = cfg.port;
          base-url = cfg.baseUrl;
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
            port = lib.mkOption {
              default = 8008;
              type = with lib.types; ints.between 0 65535;
              description = ''
                Port to listen on.
              '';
            };
            baseUrl = {
              scheme = lib.mkOption {
                default = "http";
                type = with lib.types; str;
                description = ''
                  Base URL scheme.
                '';
              };
              authority = {
                host = lib.mkOption {
                  default = "localhost";
                  type = with lib.types; str;
                  description = ''
                    Base URL host.
                  '';
                };
                port = lib.mkOption {
                  default = cfg.port;
                  type = with lib.types; nullOr (ints.between 0 65535);
                  description = ''
                    Base URL port.
                  '';
                };
              };
              path = lib.mkOption {
                default = [];
                type = with lib.types; listOf str;
                description = ''
                  Base URL path.
                '';
              };
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
            path = [
              self.packages.x86_64-linux.config # Included, because it is otherwise not seen as "buildInputs" to this service.
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
