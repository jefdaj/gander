{ mkDerivation, ansi-terminal, attoparsec, base, byteable
, bytestring, cryptonite, directory, directory-tree, docopt
, filepath, Glob, hashable, hspec, monad-parallel, pretty-simple
, process, QuickCheck, split, stdenv, streaming
, streaming-bytestring, unix, unordered-containers
}:
mkDerivation {
  pname = "Gander";
  version = "0.1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal attoparsec base byteable bytestring cryptonite
    directory directory-tree docopt filepath Glob hashable
    monad-parallel pretty-simple process split streaming
    streaming-bytestring unix unordered-containers
  ];
  executableHaskellDepends = [
    ansi-terminal attoparsec base byteable bytestring cryptonite
    directory directory-tree docopt filepath Glob hashable
    monad-parallel pretty-simple process split streaming
    streaming-bytestring unix unordered-containers
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;

  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
}
