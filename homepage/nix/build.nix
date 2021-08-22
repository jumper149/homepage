rec {
  homepage = { haskellPackages, nix-gitignore }:
    let src = nix-gitignore.gitignoreSource [] ../.;
    in haskellPackages.callCabal2nix "homepage" src {};

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
