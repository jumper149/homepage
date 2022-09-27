{ nixpkgs, setup }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    let config = builtins.fromJSON (builtins.readFile ../homepage.json);
    in stdenv.mkDerivation {
      name = "blog"; # TODO: Necessary to avoid segmentation fault.
      src = ./source;
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
              --attribute imagesdir="${(import ../server/source/library/Homepage/Configuration/BaseUrl.nix) config.base-url}/blog/raw/$ARTICLE_NAME" \
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

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
      pkgs.mkShell {
        packages = [
          pkgs.asciidoctor
        ];
      };

}
