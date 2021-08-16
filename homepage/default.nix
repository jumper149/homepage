{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  build = import ./nix/build.nix;
  pkgs = nixpkgs { };
  attrs = { inherit (pkgs) haskellPackages nix-gitignore; };
in
  build.homepage attrs
