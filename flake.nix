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
          --directory-blog ${self.packages.x86_64-linux.blog} \
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
          asciidoctor myWayToCoreboot.adoc --backend html5 --doctype article --out-file static/myWayToCoreboot.html --safe-mode secure --no-header-footer
          asciidoctor myOwnImplementationOfIExpressions.adoc --backend html5 --doctype article --out-file static/myOwnImplementationOfIExpressions.html --safe-mode secure --no-header-footer
        '';
        installPhase = ''
          cp --recursive static $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          asciidoctor
        ];
      };

    packages.x86_64-linux.files =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "files"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/files;
        buildPhase = ''
        '';
        installPhase = ''
          cp --recursive . $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          asciidoctor
        ];
      };

    packages.x86_64-linux.static =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "static"; # TODO: Necessary to avoid segmentation fault.
        src = ./static/static;
        buildPhase = ''
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

      nixosModules.homepage = { config, lib }:
      let cfg = config.services.homepage;
      in {
        options = {
          services.homepage = {
            enable = lib.mkOption {
              default = false;
              type = with lib.types; bool;
              description = ''
                Felix Springer's Homepage.
              '';
            };
          };
        };
        config = lib.mkIf cfg.enable {
          systemd.services.homepage = {
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            description = "Homepage";
            serviceConfig = {
              DynamicUser = true;
              ExecStart = "${self.defaultPackage.x86_64-linux}/homepage-full";
            };
          };
        };
      };

  };
}