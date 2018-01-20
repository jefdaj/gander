{ mkDerivation, base, bytestring, containers, cryptohash
, directory-tree, docopt, filepath, stdenv, unix
}:
mkDerivation {
  pname = "gander";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base bytestring containers cryptohash directory-tree docopt
    filepath unix
  ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;
}
