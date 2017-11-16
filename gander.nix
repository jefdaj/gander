{ mkDerivation, base, docopt, directory-tree, stdenv
, cryptohash, bytestring }:
mkDerivation {
  pname = "gander";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    docopt
    directory-tree
    cryptohash
    bytestring
  ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;
}
