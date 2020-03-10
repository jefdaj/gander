{ mkDerivation, ansi-terminal, attoparsec, base, byteable
, bytestring, bytestring-trie, cryptohash, directory
, directory-tree, docopt, filepath, Glob, hashable, hspec
, pretty-simple, process, QuickCheck, split, stdenv, unix
, unordered-containers
}:
mkDerivation {
  pname = "Gander";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal attoparsec base byteable bytestring bytestring-trie
    cryptohash directory directory-tree docopt filepath Glob hashable
    pretty-simple process split unix unordered-containers
  ];
  executableHaskellDepends = [
    ansi-terminal attoparsec base byteable bytestring bytestring-trie
    cryptohash directory directory-tree docopt filepath Glob hashable
    pretty-simple process split unix unordered-containers
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;

  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
}
