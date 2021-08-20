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
      (import ./nix/build.nix).homepage { inherit nix-gitignore haskellPackages; };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).homepageShell { inherit asciidoctor rnix-lsp haskellPackages nix-gitignore; };

  };
}
