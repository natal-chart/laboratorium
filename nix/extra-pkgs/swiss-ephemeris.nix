{ mkDerivation, base, directory, fetchgit, hpack, hspec
, hspec-discover, lib, QuickCheck, random, time, vector
}:
mkDerivation {
  pname = "swiss-ephemeris";
  version = "1.4.0.0";
  src = fetchgit {
    url = "https://github.com/lfborjas/swiss-ephemeris";
    sha256 = "1bmh760vyy36b4l86gpjix96qhsx45mqisvjw030yrcb8lljy63k";
    rev = "019fe42d8718cd24cd523ab7fae11207ec036a52";
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
