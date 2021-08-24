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
      (import ./nix/build.nix).homepage { inherit haskellPackages nix-gitignore; };

    packages.x86_64-linux.blog =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).blog { inherit asciidoctor stdenv; };

    # TODO
    packages.x86_64-linux.files =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).blog { inherit asciidoctor stdenv; };

    # TODO
    packages.x86_64-linux.static =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).static { inherit imagemagick stdenv; };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).homepageShell { inherit asciidoctor haskellPackages nix-gitignore rnix-lsp; };

  };
}
