{ nixpkgs, setup, server, config }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    writeScriptBin "homepage-full" ''
      HOMEPAGE_CONFIG_FILE="${config.packages.x86_64-linux.default}" ${server.packages.x86_64-linux.default}/bin/homepage
    '';

  packages.x86_64-linux.homepage-test-application =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    writeScriptBin "homepage-test-application-full" ''
      export HOMEPAGE_CONFIG_FILE="${config.packages.x86_64-linux.default}"
      export HOMEPAGE_LOG_LEVEL=LevelWarn
      ${server.packages.x86_64-linux.default}/bin/homepage-test-application
    '';

  overlays.default = final: prev: {
    homepage-jumper149.exe = server.packages.x86_64-linux.default;
    homepage-jumper149.full = packages.x86_64-linux.default;
    homepage-jumper149.config.default = config.config;
  };

  nixosModules.default = import ./module.nix {
    finalOverlay = overlays.default;
  };

  checks.x86_64-linux.homepage-test-application =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "homepage-test-application"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        INIT_LOG="$(homepage-test-application-full)"
        echo "$INIT_LOG"

        if [ -z "$INIT_LOG" ];
        then
          echo "Successfully checked the initialization log."
        else
          echo "Warnings/Errors/Unknowns detected in initialization log."
          echo "$INIT_LOG"
          exit 1
        fi
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
        packages.x86_64-linux.homepage-test-application
      ];
    };

}
