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

      # tutorials:
      # https://kuznero.com/post/linux/haskell-project-structure-in-nixos/
      # https://vaibhavsagar.com/blog/2018/01/03/static-haskell-nix/

      # TODO debug version with profiling enabled too

      # MacOS 10.13.6 says...
      #
      # otool -L ./result/bin/gander
      # ./result/bin/gander:
      #   /nix/store/h144jawqa92rqjhaahrsikq5j2dwkh5n-Libsystem-osx-10.12.6/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1226.10.1)
      #   /nix/store/v0n14y5q17p050a81ih6w5d0szsncjmv-libiconv-osx-10.12.6/lib/libiconv.dylib (compatibility version 7.0.0, current version 7.0.0)
      #   /nix/store/nfw1pxhxc3f442i4n87syxw1jp86xj74-gmp-6.2.0/lib/libgmp.10.dylib (compatibility version 15.0.0, current version 15.0.0)
      #   /nix/store/n2lnwpa32agnbskfs38v4i5hn43j5n08-libffi-3.3/lib/libffi.7.dylib (compatibility version 9.0.0, current version 9.0.0)
      #
      # And if you try to run it on a different laptop without Nix...
      #
      # dyld: Library not loaded: /nix/store/nfw1pxhxc3f442i4n87syxw1jp86xj74-gmp-6.2.0/lib/libgmp.10.dylib
      # Referenced from: /Volumes/PIPETTOR/./gander
      # Reason: image not found
      #
      gander-static =
        overrideCabal
          (justStaticExecutables hpNew.gander)
          (oldDerivation: {
            configureFlags = [
              # TODO figure these out
              # "--ghc-option=-optl=-static"
              # "--ghc-option=-optl=-L${pkgs.gmp6.static}/lib"
              # "--ghc-option=-optl=-pthread" # TODO remove?
              # "--ghc-option=-optl=-L${pkgs.zlib.static}/lib" # TODO remove?
              # "--ghc-option=-optl=-L${pkgs.gmp5.static}/lib"
              # "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
            ];
          });
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
