{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "files"; # TODO: Necessary to avoid segmentation fault.
      src = ./source;
      buildPhase = ''
      '';
      installPhase = ''
        cp --recursive . $out
      '';
    };

  entries = builtins.fromJSON (builtins.readFile ./entries.json);

}
