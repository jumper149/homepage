{
  description = "Felix Springer's Homepage";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    monad-control-identity = {
      type = "github";
      owner = "jumper149";
      repo = "monad-control-identity";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deriving-trans = {
      type = "github";
      owner = "jumper149";
      repo = "deriving-trans";
      ref = "monad-control-identity";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.monad-control-identity.follows = "monad-control-identity";
    };
    wai-control = {
      type = "github";
      owner = "jumper149";
      repo = "wai-control";
      ref = "v0.2.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, monad-control-identity, deriving-trans, wai-control }: {

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      writeScriptBin "homepage-full" ''
        HOMEPAGE_CONFIG_FILE="${self.packages.x86_64-linux.config}" ${self.packages.x86_64-linux.homepage}/bin/homepage
      '';

    packages.x86_64-linux.config =
      with import nixpkgs { system = "x86_64-linux"; };
      writeText "homepage.json" (builtins.toJSON self.config);

    config =
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
          monad-control-identity = self.callCabal2nix "monad-control-identity" monad-control-identity.outPath {};
          deriving-trans = self.callCabal2nix "deriving-trans" deriving-trans.outPath {};
          wai-control = self.callCabal2nix "wai-control" wai-control.outPath {};
        };
      in (haskellPackages.extend overlay).callCabal2nixWithOptions "homepage" src "-fcabal2nix" {};

    packages.x86_64-linux.blog =
      with import nixpkgs { system = "x86_64-linux"; };
      let config = builtins.fromJSON (builtins.readFile ./homepage.json);
      in stdenv.mkDerivation {
        name = "blog"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/blog;
        # TODO: Use `base-url` to set `homepage`.
        buildPhase = ''
          mkdir -p static

          ASCIIDOCTOR_FLAG_LIST=(
            "--doctype article"
            "--safe-mode server"
            "--attribute source-highlighter=rouge"
            "--attribute prewrap!"
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
                --attribute author="${config.contact-information.name}" \
                --attribute homepage="https://felixspringer.xyz[${config.contact-information.homepage-label}]" \
                --attribute imagesdir="${(import source/library/Homepage/Configuration/BaseUrl.nix) config.base-url}/blog/raw/$ARTICLE_NAME" \
                --backend html5 \
                --attribute nofooter \
                --attribute webfonts!
              sed -i 's/^<head>$/<head>\n<base target="_parent">/' "static/$ARTICLE_NAME.html"

              echo "PDF: '$ARTICLE_NAME'"
              asciidoctor-pdf "$ARTICLE_NAME.adoc" --out-file "static/$ARTICLE_NAME.pdf" $ASCIIDOCTOR_FLAGS \
                --attribute author="${config.contact-information.name}" \
                --attribute homepage="https://felixspringer.xyz[${config.contact-information.homepage-label}]" \
                --attribute imagesdir="$ARTICLE_NAME" \
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

          echo "Remove 'favicon.xpm'."
          rm favicon.xpm

          echo "Add link 'favicon.png'."
          ln -s favicon-32x32.png favicon.png

          echo "Add link 'favicon.ico'."
          ln -s favicon.png favicon.ico

          resizeIcon() {
            ICON_NAME="$1"
            if [ -f "$ICON_NAME" ]
            then
              echo "Icon exists: '$ICON_NAME'."

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

          echo "Compile CSS."
          lessc stylesheet.less stylesheet.css

          echo "Remove 'stylesheet.less'."
          rm stylesheet.less
        '';
        installPhase = ''
          mkdir -p $out
          cp --recursive . $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          imagemagick
          lessc
        ];
      };

    checks.x86_64-linux.fourmolu =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "fourmolu"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          fourmolu --cabal-default-extensions --mode check $(find source -name '*.hs')
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.fourmolu
        ];
      };

    checks.x86_64-linux.hlint =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "hlint"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          hlint ./source
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.hlint
        ];
      };

    checks.x86_64-linux.hie-yaml =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "hie-yaml"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          diff --report-identical-files ./hie.yaml <(gen-hie)
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.implicit-hie
        ];
      };

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          asciidoctor
          blaze-from-html
          cabal-install
          calligraphy
          pkgs.findutils
          fourmolu
          ghcid
          graphmod
          haskell-language-server
          hlint
          hnix
          pkgs.imagemagick
          implicit-hie
          lessc
          rnix-lsp
          weeder
          pkgs.xdot
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.homepage
        ];
        withHoogle = true;
      };

    nixosModule = { config, lib, ... }:
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
              ExecStart = "${self.packages.x86_64-linux.homepage}/bin/homepage";
              LogsDirectory = "homepage";
            };
          };
        };
      };

  };
}
