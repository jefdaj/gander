{ mkDerivation, ansi-terminal, attoparsec, base, base64-bytestring
, byteable, bytestring, cryptonite, directory, directory-tree
, docopt, filepath, Glob, hashable, hashtables, hspec
, monad-parallel, pretty-simple, process, QuickCheck
, safe-exceptions, split, stdenv, store, streaming
, streaming-bytestring, text, th-utilities, unix
, unordered-containers
}:
mkDerivation {
  pname = "Gander";
  version = "0.1.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal attoparsec base base64-bytestring byteable bytestring
    cryptonite directory directory-tree docopt filepath Glob hashable
    hashtables monad-parallel pretty-simple process safe-exceptions
    split store streaming streaming-bytestring text th-utilities unix
    unordered-containers
  ];
  executableHaskellDepends = [
    ansi-terminal attoparsec base base64-bytestring byteable bytestring
    cryptonite directory directory-tree docopt filepath Glob hashable
    hashtables monad-parallel pretty-simple process safe-exceptions
    split store streaming streaming-bytestring text th-utilities unix
    unordered-containers
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;

  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
}
