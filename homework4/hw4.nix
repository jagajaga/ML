{ stdenv, boost, cmake }:

stdenv.mkDerivation rec {
  name = "hw4";
  src = "linear_regression";
  buildInputs = [
    boost cmake
  ];
}
