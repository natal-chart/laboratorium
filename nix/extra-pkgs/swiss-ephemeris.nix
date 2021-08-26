{ mkDerivation, base, directory, fetchgit, hpack, hspec
, hspec-discover, lib, QuickCheck, random, time, vector
}:
mkDerivation {
  pname = "swiss-ephemeris";
  version = "1.4.0.0";
  src = fetchgit {
    url = "https://github.com/lfborjas/swiss-ephemeris";
    sha256 = "1zq8frj21p9np2girkjcwsd25q066bzbk5b70h4rgzy49mw5mm5z";
    rev = "b8300aef29aef55be833d778712bc816d78858ac";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base time vector ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base directory hspec QuickCheck random time vector
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/lfborjas/swiss-ephemeris#readme";
  description = "Haskell bindings for the Swiss Ephemeris C library";
  license = lib.licenses.agpl3;
}
