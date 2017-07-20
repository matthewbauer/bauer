{ mkDerivation, aeson, base, bytestring, containers, data-default
, directory, filepath, hashable, hslogger, hspec, lens, mtl, parsec
, lib, stm, text, time, transformers, unordered-containers
, vector, yi-rope, fetchFromGitHub
}:
mkDerivation {
  pname = "haskell-lsp";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "alanz";
    repo = "haskell-lsp";
    rev = "f32e05d873ee4b5c58ba79e5e4113fb5ebf73f7b";
    sha256 = "0frqbciqzxi3nnsmcy4r263m03pcqfc73dbzbvw97a9wybrczm0q";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default directory filepath
    hashable hslogger lens mtl parsec stm text time
    unordered-containers yi-rope
  ];
  executableHaskellDepends = [
    aeson base bytestring containers data-default directory filepath
    hslogger lens mtl parsec stm text time transformers
    unordered-containers vector yi-rope
  ];
  testHaskellDepends = [
    aeson base containers directory hashable hspec lens text yi-rope
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol";
  license = lib.licenses.mit;
}
