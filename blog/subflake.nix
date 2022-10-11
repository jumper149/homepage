{ self, nixpkgs, setup }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "blog"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        source ${packages.x86_64-linux.initBuildEnvironment}

        compileArticle() {
          ARTICLE_NAME="$1"
          make "build/$ARTICLE_NAME.html"
          make "build/$ARTICLE_NAME.pdf"
          if [ -d "source/$ARTICLE_NAME" ]
          then
            echo "Additional directory exists: '$ARTICLE_NAME'"
            cp -r source/$ARTICLE_NAME build
          else
            echo "Additional directory doesn't exist: '$ARTICLE_NAME'"
          fi
        }

        for blogId in ${lib.strings.escapeShellArgs (builtins.attrNames entries)}
        do
          compileArticle "$blogId"
        done
      '';
      installPhase = ''
        cp --recursive build $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        asciidoctor
      ];
    };

  packages.x86_64-linux.initBuildEnvironment =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    writeShellScript "environmentVariables" ''
      export AUTHOR="${setup.config.contact-information.name}"
      export BASEURL="${import ./base-url.nix setup.config.base-url}"
      export EMAIL="${setup.config.contact-information.email-address}"
      export HOMEPAGE="${import ./base-url.nix setup.config.base-url}[${setup.config.contact-information.homepage-label}]"
      export REVNUMBER="${if self ? rev then self.rev else "unknown-revision"}"
    '';

  config = builtins.fromJSON (builtins.readFile ./config.json);

  entries = builtins.fromJSON (builtins.readFile ./entries.json);

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    pkgs.mkShell {
      packages = [
        pkgs.asciidoctor
      ];
      shellHook = ''
        source ${packages.x86_64-linux.initBuildEnvironment}
      '';
    };

}
