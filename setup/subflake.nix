{ }: rec {

  overlays.default = final: prev: {
    haskellPackages = prev.haskell.packages.ghc924.extend (haskellFinal: haskellPrev: { # TODO: Using GHC 9.2.4.
      singletons-th = haskellPrev.callHackage "singletons-th" "3.1" {}; # TODO: Required for GHC 9.2.
      graphmod = (haskellPrev.graphmod.overrideAttrs (oldAttrs: {
        src = prev.fetchFromGitHub {
          owner = "jumper149";
          repo = "graphmod";
          rev = "b684ce4d6af97179eccb65d2567d6165d43fa3e0";
          sha256 = "sha256-I5OfUGV9TbxLCyc8LhdZODhw5EpJXyXeFdaN7gMmhC8=";
        };
      }));
    });
  };

  config = builtins.fromJSON (builtins.readFile ./nixpublic.json);

}
