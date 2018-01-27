{ mkDerivation, base, bytestring, containers, cryptohash
, directory-tree, docopt, filepath, Glob, stdenv, unix
}:
mkDerivation {
  pname = "gander";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers cryptohash directory-tree docopt
    filepath Glob unix
  ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;
}
