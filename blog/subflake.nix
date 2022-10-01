{ self, nixpkgs, setup }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "blog"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        mkdir -p static

        ASCIIDOCTOR_FLAG_LIST=(
          "--doctype article"
          "--safe-mode server"
          "--attribute source-highlighter=rouge"
          "--attribute prewrap!"
          "--attribute email=${config.email-address}"
          "--attribute revnumber="${if self ? rev then self.rev else "unknown-revision"}""
        )
        ASCIIDOCTOR_FLAGS="$(for flag in "''${ASCIIDOCTOR_FLAG_LIST[*]}"; do echo $flag; done)"

        compileArticle() {
          ARTICLE_NAME="$1"

          if [ -f "source/$ARTICLE_NAME.adoc" ]
          then
            echo "Article exists: '$ARTICLE_NAME'"

            echo "HTML: '$ARTICLE_NAME'"
            asciidoctor "source/$ARTICLE_NAME.adoc" --out-file "static/$ARTICLE_NAME.html" $ASCIIDOCTOR_FLAGS \
              --attribute author="${config.name}" \
              --attribute homepage="${config.homepage-url}[${config.homepage-label}]" \
              --attribute imagesdir="${config.base-url}/blog/raw/$ARTICLE_NAME" \
              --backend html5 \
              --attribute nofooter \
              --attribute webfonts!
            sed -i 's/^<head>$/<head>\n<base target="_parent">/' "static/$ARTICLE_NAME.html"

            echo "PDF: '$ARTICLE_NAME'"
            asciidoctor-pdf "source/$ARTICLE_NAME.adoc" --out-file "static/$ARTICLE_NAME.pdf" $ASCIIDOCTOR_FLAGS \
              --attribute author="${config.name}" \
              --attribute homepage="${config.homepage-url}[${config.homepage-label}]" \
              --attribute imagesdir="$ARTICLE_NAME" \
              --attribute pdf-theme="pdf-theme.yml"

            if [ -d "source/$ARTICLE_NAME" ]
            then
              echo "Additional directory exists: '$ARTICLE_NAME'"
              cp -r source/$ARTICLE_NAME static
            else
              echo "Additional directory doesn't exist: '$ARTICLE_NAME'"
            fi
            else
              echo "Article doesn't exist: '$ARTICLE_NAME'"
              exit 2
          fi
        }

        for blogId in ${lib.strings.escapeShellArgs (builtins.attrNames entries)}
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

  config = builtins.fromJSON (builtins.readFile ./config.json);

  entries = builtins.fromJSON (builtins.readFile ./entries.json);

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    pkgs.mkShell {
      packages = [
        pkgs.asciidoctor
        (
          writeScriptBin "asciidoctor-pdf-blog" ''
            ARTICLE_NAME="$1"

            ASCIIDOCTOR_FLAG_LIST=(
              "--doctype article"
              "--safe-mode server"
              "--attribute source-highlighter=rouge"
              "--attribute prewrap!"
              "--attribute email=${config.email-address}"
              "--attribute revnumber="unknown-revision""
            )
            ASCIIDOCTOR_FLAGS="$(for flag in "''${ASCIIDOCTOR_FLAG_LIST[*]}"; do echo $flag; done)"

            asciidoctor-pdf "source/$ARTICLE_NAME.adoc" --out-file "out/$ARTICLE_NAME.pdf" $ASCIIDOCTOR_FLAGS \
              --attribute author="${config.name}" \
              --attribute homepage="${config.homepage-url}[${config.homepage-label}]" \
              --attribute imagesdir="$ARTICLE_NAME" \
              --attribute pdf-theme="pdf-theme.yml"
          ''
        )
      ];
    };

}
