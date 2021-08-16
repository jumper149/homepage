{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  build = import ./nix/build.nix;
  pkgs = nixpkgs { };
  attrs = { inherit (pkgs) rnix-lsp haskellPackages nix-gitignore; };
in
  build.homepageShell attrs
