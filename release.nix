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
      # Similarly, on linux it builds but ldd says there are still linked libraries:
      # ldd result/bin/gander 
      #   linux-vdso.so.1 (0x00007ffc2a168000)
      #   libm.so.6 => /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27/lib/libm.so.6 (0x00007f8d53450000)
      #   libpthread.so.0 => /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27/lib/libpthread.so.0 (0x00007f8d5342f000)
      #   librt.so.1 => /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27/lib/librt.so.1 (0x00007f8d53425000)
      #   libutil.so.1 => /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27/lib/libutil.so.1 (0x00007f8d53420000)
      #   libdl.so.2 => /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27/lib/libdl.so.2 (0x00007f8d5341b000)
      #   libgmp.so.10 => /nix/store/sisy13ic6giv9yn0fyl2n9cpm84xscvx-gmp-6.1.2/lib/libgmp.so.10 (0x00007f8d53383000)
      #   libffi.so.6 => /nix/store/13k46bsrkqczmy5vxsqbvvs58jkmha9z-libffi-3.2.1/lib/libffi.so.6 (0x00007f8d53377000)
      #   libc.so.6 => /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27/lib/libc.so.6 (0x00007f8d531c1000)
      #   /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27/lib/ld-linux-x86-64.so.2 => /nix/store/bpgdx6qqqzzi3szb0y3di3j3660f3wkj-glibc-2.31/lib64/ld-linux-x86-64.so.2 (0x00007f8d535e8000)
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
