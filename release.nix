let
  sources = import ./nix/sources.nix;
  pkgs1   = import <nixpkgs> {};
  pkgs2   = import (if pkgs1.stdenv.isDarwin then sources.nixpkgs-darwin else sources.nixpkgs) {};
in
{ compiler ? "ghc884"
, pkgs ? pkgs2
}:

let
  inherit (pkgs.haskell.lib) markUnbroken dontCheck overrideCabal;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {

      niv       = import sources.niv {};
      scheduler = markUnbroken hpOld.scheduler;
      massiv    = markUnbroken hpOld.massiv;
      docopt    = markUnbroken (hpNew.callCabal2nix "docopt" sources.docopt {});

      gander = overrideCabal (hpNew.callCabal2nix "gander" ./. {}) (_: {
        executableSystemDepends = with pkgs; [
          gitAndTools.git
          gitAndTools.gitAnnex
          rsync
        ];
      });

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
      hlint # or ormolu
      # niv
      # pkgs.cacert # needed for niv
      # pkgs.nix    # needed for niv
    ];
    withHoogle = true;
  };
}
