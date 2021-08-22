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
      (import ./nix/build.nix).homepage { inherit haskellPackages nix-gitignore; };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).homepageShell { inherit asciidoctor haskellPackages nix-gitignore rnix-lsp; };

  };
}
