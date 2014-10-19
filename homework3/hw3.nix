{ cabal, deepseq, text, vector, repa, matrix }:

cabal.mkDerivation (self: {
  pname = "hw3";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    deepseq text vector repa matrix
  ];
})
