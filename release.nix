let
  sources = import ./nix/sources.nix;
in
{ compiler ? "ghc884"
, pkgs ? import sources.nixpkgs { }
}:

let
  inherit (pkgs.haskell.lib) markUnbroken overrideCabal justStaticExecutables;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      niv       = import sources.niv {};
      scheduler = markUnbroken hpOld.scheduler;
      massiv    = markUnbroken hpOld.massiv;
      docopt    = markUnbroken (hpNew.callCabal2nix "docopt" sources.docopt {});
      gander    = hpNew.callCabal2nix "Gander" ./. {};

      # on osx 10.13.6, this seems to work:
      gander-static = justStaticExecutables hpNew.gander;

      # on linux, is the rest of this needed?
      # gander-static =
      #   overrideCabal
      #     (justStaticExecutables hpNew.gander);
      #     (oldDerivation: {
      #       configureFlags = [
      #         "--ghc-option=-optl=-static"
      #         "--ghc-option=-optl=-pthread"
      #         "--ghc-option=-optl=-L${pkgs.gmp5.static}/lib"
      #         "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
      #         "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
      #       ];
      #     });
    };
  };
  project = haskellPackages.gander;
  static  = haskellPackages.gander-static;
in
{
  # TODO what are these for?
  project = project;
  static  = static;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      project
    ];
    buildInputs = with haskellPackages; [
      ghcid
      hlint       # or ormolu
      # niv
      # pkgs.cacert # needed for niv
      # pkgs.nix    # needed for niv
    ];
    withHoogle = true;
  };
}
