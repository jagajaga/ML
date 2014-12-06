{ cabal  }:

cabal.mkDerivation (self: {
  pname = "perceptron";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
  ];
})
