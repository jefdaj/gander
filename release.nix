let
  sources = import ./nix/sources.nix;
in
{ compiler ? "ghc884"
, pkgs ? import sources.nixpkgs { }
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      niv    = import sources.niv { };
      scheduler = pkgs.haskell.lib.markUnbroken hpOld.scheduler;
      massiv = pkgs.haskell.lib.markUnbroken hpOld.massiv;
      docopt = pkgs.haskell.lib.markUnbroken (hpNew.callCabal2nix "docopt" sources.docopt {});
      gander = hpNew.callCabal2nix "Gander" ./. { };
    };
  };
  project = haskellPackages.gander;
in
{
  project = project;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      project
    ];
    buildInputs = with haskellPackages; [
      ghcid
      hlint       # or ormolu
      niv
      pkgs.cacert # needed for niv
      pkgs.nix    # needed for niv
    ];
    withHoogle = true;
  };
}
