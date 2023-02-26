{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "files"; # TODO: Necessary to avoid segmentation fault.
      src = ./source;
      buildPhase = ''
      '';
      installPhase = ''
        id3v2 --delete-all "Felix_Springer-Unitary.flac"
        id3v2 --artist "Felix Springer" --song "Unitary" --year "2022" "Felix_Springer-Unitary.flac"

        id3v2 --delete-all "Felix_Springer-Unitary.mp3"
        id3v2 --artist "Felix Springer" --song "Unitary" --year "2022" "Felix_Springer-Unitary.mp3"

        id3v2 --delete-all "Felix_Springer-Unitary.wav"
        id3v2 --artist "Felix Springer" --song "Unitary" --year "2022" "Felix_Springer-Unitary.wav"

        id3v2 --delete-all "Felix_Springer-Fido.flac"
        id3v2 --artist "Felix Springer" --song "Fido" --year "2022" "Felix_Springer-Fido.flac"

        id3v2 --delete-all "Felix_Springer-Fido.mp3"
        id3v2 --artist "Felix Springer" --song "Fido" --year "2022" "Felix_Springer-Fido.mp3"

        id3v2 --delete-all "Felix_Springer-Fido.wav"
        id3v2 --artist "Felix Springer" --song "Fido" --year "2022" "Felix_Springer-Fido.wav"

        id3v2 --delete-all "Felix_Springer-L2.flac"
        id3v2 --artist "Felix Springer" --song "L2" --year "2023" "Felix_Springer-L2.flac"

        id3v2 --delete-all "Felix_Springer-L2.mp3"
        id3v2 --artist "Felix Springer" --song "L2" --year "2023" "Felix_Springer-L2.mp3"

        id3v2 --delete-all "Felix_Springer-L2.wav"
        id3v2 --artist "Felix Springer" --song "L2" --year "2023" "Felix_Springer-L2.wav"

        id3v2 --delete-all "Felix_Springer-Naptune.flac"
        id3v2 --artist "Felix Springer" --song "Naptune" --year "2023" "Felix_Springer-Naptune.flac"

        id3v2 --delete-all "Felix_Springer-Naptune.mp3"
        id3v2 --artist "Felix Springer" --song "Naptune" --year "2023" "Felix_Springer-Naptune.mp3"

        id3v2 --delete-all "Felix_Springer-Naptune.wav"
        id3v2 --artist "Felix Springer" --song "Naptune" --year "2023" "Felix_Springer-Naptune.wav"

        cp --recursive . $out
      '';
      nativeBuildInputs = [
        pkgs.id3v2
      ];
    };

  entries = builtins.fromJSON (builtins.readFile ./entries.json);

}
