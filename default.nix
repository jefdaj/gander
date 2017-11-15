{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

# TODO include git-annex as a dep? (use machinery from shortcut)

let
  inherit (nixpkgs) pkgs;
  f = import ./gander.nix;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv
