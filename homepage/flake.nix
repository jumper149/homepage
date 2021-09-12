{
  description = "Felix Springer's Homepage";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      writeScript "homepage-full" ''
        #!${pkgs.bash}/bin/bash
        ${self.packages.x86_64-linux.homepage}/bin/homepage \
          --directory-blog ${self.packages.x86_64-linux.blog}/static/blog \
          --directory-files ${self.packages.x86_64-linux.files} \
          --directory-static ${self.packages.x86_64-linux.static}
      '';

    packages.x86_64-linux.homepage =
      with import nixpkgs { system = "x86_64-linux"; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskellPackages.callCabal2nix "homepage" src {};

    packages.x86_64-linux.blog =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "blog"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/blog;
        buildPhase = ''
          mkdir -p static
          asciidoctor myWayToCoreboot.adoc --backend html5 --doctype article --out-file blog/myWayToCoreboot.html --safe-mode secure --no-header-footer
          asciidoctor myOwnImplementationOfIExpressions.adoc --backend html5 --doctype article --out-file blog/myOwnImplementationOfIExpressions.html --safe-mode secure --no-header-footer
        '';
        installPhase = ''
          mkdir -p $out/static
          cp --recursive blog $out/static
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          asciidoctor
        ];
      };

    # TODO
    packages.x86_64-linux.files =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "blog"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/blog;
        buildPhase = ''
          mkdir -p static
          asciidoctor myWayToCoreboot.adoc -o blog/myWayToCoreboot.html
          asciidoctor myOwnImplementationOfIExpressions.adoc -o blog/myOwnImplementationOfIExpressions.html
        '';
        installPhase = ''
          mkdir -p $out/static
          cp --recursive blog $out/static
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          asciidoctor
        ];
      };

    # TODO
    packages.x86_64-linux.static =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "static"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/static;
        buildPhase = ''
          mkdir -p static
          convert favicon.xpm favicon.png
        '';
        installPhase = ''
          mkdir -p $out
          cp favicon.png $out/favicon.png
          cp stylesheet.css $out/stylesheet.css
          cp asciidoctor.css $out/asciidoctor.css
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          imagemagick
        ];
      };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          asciidoctor
          blaze-from-html
          ghcid
          haskell-language-server
          hlint
          hnix
          implicit-hie
          rnix-lsp
          weeder
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.homepage
        ];
        withHoogle = true;
      };

  };
}
