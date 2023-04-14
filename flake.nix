# based on https://discourse.nixos.org/t/another-simple-flake-for-haskell-development/18164

# TODO why can't the tests find git-annex? or are they just failing separately?

{

  inputs = {
    nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system}.pkgsStatic;
      let
        t = lib.trivial;
        hl = haskell.lib;

        name = "gander";

        project = devTools:
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in haskellPackages.developPackage {
            root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ".txt" ];
            name = name;
            returnShellEnv = !(devTools == [ ]);

            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };

      in rec {

        # empty devTools tells it to build the package
        packages.pkg = project [ ];

        # use `nix build` to build the static binary
        defaultPackage = self.packages.${system}.pkg;

        # use `nix build .#docker` to build the docker image
        # TODO is it missing something? how can it be smaller than the binary?
        packages.docker = pkgs.dockerTools.buildImage {
          name = "gander";
          config = {
            Cmd = [ "${packages.pkg}/bin/gander" ];
          };
        };

        executableSystemDepends = [
          gitAndTools.git
          gitAndTools.gitAnnex
          rsync
        ];

        devShell = project (executableSystemDepends ++ (with haskellPackages; [
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
        ]));
      });
}
