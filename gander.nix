{ mkDerivation, base, docopt, directory-tree, stdenv, filepath
, cryptohash, bytestring, containers, unix }:
mkDerivation {
  pname = "gander";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    containers
    docopt
    directory-tree
    cryptohash
    bytestring
    filepath
    unix
  ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;
}
