{
  description = "Felix Springer's Homepage";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    deriving-trans = {
      type = "github";
      owner = "jumper149";
      repo = "deriving-trans";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, deriving-trans }: {

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      let config = writeText "homepage.json" (builtins.toJSON self.defaultConfig.x86_64-linux);
      in writeScriptBin "homepage-full" ''
        HOMEPAGE_CONFIG_FILE="${config}" ${self.packages.x86_64-linux.homepage}/bin/homepage
      '';

    defaultConfig.x86_64-linux =
      builtins.fromJSON (builtins.readFile ./homepage.json) // {
        revision = if self ? rev then self.rev else null;
        directory-blog = "${self.packages.x86_64-linux.blog}";
        directory-files = "${self.packages.x86_64-linux.files}";
        directory-static = "${self.packages.x86_64-linux.static}";
      };

    packages.x86_64-linux.homepage =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        src = nix-gitignore.gitignoreSource [] ./.;
        overlay = self: super: {
          deriving-trans = deriving-trans.defaultPackage.x86_64-linux;
        };
      in (haskellPackages.extend overlay).callCabal2nix "homepage" src {};

    packages.x86_64-linux.blog =
      with import nixpkgs { system = "x86_64-linux"; };
      let config = builtins.fromJSON (builtins.readFile ./homepage.json);
      in stdenv.mkDerivation {
        name = "blog"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/blog;
        # TODO: Configure `author` and `homepage`.
        buildPhase = ''
          mkdir -p static

          ASCIIDOCTOR_FLAG_LIST=(
            "--doctype article"
            "--safe-mode server"
            "--attribute source-highlighter=rouge"
            "--attribute email=${config.contact-information.email-address}"
            "--attribute revnumber="${if self ? rev then self.rev else "unknown-revision"}""
          )
          ASCIIDOCTOR_FLAGS="$(for flag in "''${ASCIIDOCTOR_FLAG_LIST[*]}"; do echo $flag; done)"

          compileArticle() {
            ARTICLE_NAME="$1"

            if [ -f "$ARTICLE_NAME.adoc" ]
            then
              echo "Article exists: '$ARTICLE_NAME'"

              echo "HTML: '$ARTICLE_NAME'"
              asciidoctor "$ARTICLE_NAME.adoc" --out-file "static/$ARTICLE_NAME.html" $ASCIIDOCTOR_FLAGS \
                --attribute author="Felix Springer" \
                --attribute homepage="https://felixspringer.xyz[felixspringer.xyz]" \
                --backend html5 \
                --attribute nofooter
              sed -i 's/^<head>$/<head>\n<base target="_parent">/' "static/$ARTICLE_NAME.html"

              echo "PDF: '$ARTICLE_NAME'"
              asciidoctor-pdf "$ARTICLE_NAME.adoc" --out-file "static/$ARTICLE_NAME.pdf" $ASCIIDOCTOR_FLAGS \
                --attribute author="Felix Springer" \
                --attribute homepage="https://felixspringer.xyz[felixspringer.xyz]" \
                --attribute pdf-theme="style/pdf-theme.yml"

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

          for blogId in ${lib.strings.escapeShellArgs (builtins.attrNames config.blog-entries)}
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
          FAVICON_RESOLUTIONS=(
            "32x32"
            "192x192"
            "512x512"
          )
          for resolution in ''${FAVICON_RESOLUTIONS[*]}
          do
            echo "Build 'favicon-''${resolution}.png'."
            convert favicon.xpm -scale "''${resolution}" "favicon-''${resolution}.png"
          done

          echo "Remove 'favicon.xpm'"
          rm favicon.xpm

          echo "Add link 'favicon.png'."
          ln -s favicon-32x32.png favicon.png

          echo "Add link 'favicon.ico'."
          ln -s favicon.png favicon.ico

          resizeIcon() {
            ICON_NAME="$1"
            if [ -f "$ICON_NAME" ]
            then
              echo "Icon exists: '$ICON_NAME'"

              echo "Resize icon: '$ICON_NAME'."
              convert "$ICON_NAME" -resize 128x128 "$ICON_NAME"
            else
              echo "Icon doesn't exist: '$ICON_NAME'"
              exit 2
            fi
          }

          for icon in ./icons/*
          do
            resizeIcon "$icon"
          done
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
        homepageConfig = self.defaultConfig.x86_64-linux // {
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
