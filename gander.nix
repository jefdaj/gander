{ mkDerivation, ansi-terminal, attoparsec, base, byteable
, bytestring, containers, cryptohash, directory, directory-tree
, docopt, filepath, Glob, hspec, pretty-simple, process, QuickCheck
, split, stdenv, unix
}:
mkDerivation {
  pname = "Gander";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal attoparsec base byteable bytestring containers
    cryptohash directory directory-tree docopt filepath Glob
    pretty-simple process split unix
  ];
  executableHaskellDepends = [
    ansi-terminal attoparsec base byteable bytestring containers
    cryptohash directory directory-tree docopt filepath Glob
    pretty-simple process split unix
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;

  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
}
