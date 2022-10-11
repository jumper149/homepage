{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "blog"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        source ${packages.x86_64-linux.initBuildEnvironment}

        for blogId in ${lib.strings.escapeShellArgs (builtins.attrNames entries)}
        do
          make "build/$blogId.html"
          make "build/$blogId.pdf"
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
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeShellScript "environmentVariables" ''
      export AUTHOR="${self.subflakes.setup.config.contact-information.name}"
      export BASEURL="${import ./base-url.nix self.subflakes.setup.config.base-url}"
      export EMAIL="${self.subflakes.setup.config.contact-information.email-address}"
      export HOMEPAGE="${import ./base-url.nix self.subflakes.setup.config.base-url}[${self.subflakes.setup.config.contact-information.homepage-label}]"
      export REVNUMBER="${if self ? rev then self.rev else "unknown-revision"}"
    '';

  entries = builtins.fromJSON (builtins.readFile ./entries.json);

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    pkgs.mkShell {
      inputsFrom = [
        packages.x86_64-linux.default
      ];
      shellHook = ''
        source ${packages.x86_64-linux.initBuildEnvironment}
      '';
    };

}
