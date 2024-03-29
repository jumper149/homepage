{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "static"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        make all

        cp -r ${packages.x86_64-linux.fonts}/fonts build
      '';
      installPhase = ''
        cp --recursive build $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        imagemagick
        lessc
      ];
    };

  packages.x86_64-linux.fonts =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "fonts"; # TODO: Necessary to avoid segmentation fault.
      dontUnpack = true;
      buildPhase = ''
        mkdir -p build
        cp -r ${pkgs.fira.outPath}/share/fonts/opentype build
        mv build/opentype build/fonts
        chmod --recursive +w build/fonts
        for f in build/fonts/*
        do
          woff2_compress $f
        done
      '';
      installPhase = ''
        cp --recursive build $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        woff2
      ];
    };

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    pkgs.mkShell {
      inputsFrom = [
        packages.x86_64-linux.default
      ];
    };

}
