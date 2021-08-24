rec {
  homepage = { haskellPackages, nix-gitignore }:
    let src = nix-gitignore.gitignoreSource [] ../.;
    in haskellPackages.callCabal2nix "homepage" src {};

  blog = { asciidoctor, stdenv }:
    stdenv.mkDerivation {
      name = "blog"; # TODO: Necessary to avoid segmentation fault.
      src = ../static/blog;
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

  static = { imagemagick, stdenv }:
    stdenv.mkDerivation {
      name = "static"; # TODO: Necessary to avoid segmentation fault.
      src = ../static/static;
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

  homepageShell = { asciidoctor, haskellPackages, nix-gitignore, rnix-lsp }:
    haskellPackages.shellFor {
      buildInputs = with haskellPackages; [
        asciidoctor
        blaze-from-html
        haskell-language-server
        hlint
        hnix
        implicit-hie
        rnix-lsp
      ];
      packages = haskellPackages: [
        (homepage { inherit haskellPackages nix-gitignore; })
      ];
      withHoogle = true;
    };
}
