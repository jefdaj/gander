{ mkDerivation, base, bytestring, containers, cryptohash
, directory-tree, docopt, filepath, Glob, stdenv, unix, pretty-simple
}:
mkDerivation {
  pname = "gander";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers cryptohash directory-tree docopt
    filepath Glob unix pretty-simple
  ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;
}
