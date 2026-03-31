{ self }: rec {

  overlays.default = final: prev: {
    haskellPackages = prev.haskellPackages.extend (haskellFinal: haskellPrev: {
      wai-control = (haskellPrev.wai-control.overrideAttrs (oldAttrs: {
        meta.broken = false;
        src = prev.fetchFromGitHub {
          owner = "jumper149";
          repo = "wai-control";
          rev = "829c252c284e1153fc60b8a4358185d9d932959b";
          sha256 = "sha256-2lFwVyeF45x1PO1VKjbJJdR3Q6QTArcqxgURjT8Khmc=";
        };
      }));
    });
  };

  deployment = builtins.fromJSON (builtins.readFile ./deployment.json);

  config = builtins.fromJSON (builtins.readFile ./configurations/${deployment}.json);

}
