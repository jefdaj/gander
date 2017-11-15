{ mkDerivation, base, docopt, directory-tree, stdenv }:
mkDerivation {
  pname = "gander";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base docopt directory-tree ];
  description = "The \"Git ANnex DEdupeR\"";
  license = stdenv.lib.licenses.lgpl3;
}
