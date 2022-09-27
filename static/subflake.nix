{ nixpkgs, setup }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "static"; # TODO: Necessary to avoid segmentation fault.
      src = ./static;
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

}
