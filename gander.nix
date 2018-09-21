{ mkDerivation, ansi-terminal, attoparsec, base, bytestring
, containers, cryptohash, directory, directory-tree, docopt
, filepath, Glob, MissingH, pretty-simple, process, split, stdenv
, unix
}:
mkDerivation {
  pname = "Gander";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    ansi-terminal attoparsec base bytestring containers cryptohash
    directory directory-tree docopt filepath Glob MissingH
    pretty-simple process split unix
  ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;
}
