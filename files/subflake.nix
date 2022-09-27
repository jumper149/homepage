{ nixpkgs, setup }: {

  packages.x86_64-linux.files =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "files"; # TODO: Necessary to avoid segmentation fault.
      src = ./source;
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

}
