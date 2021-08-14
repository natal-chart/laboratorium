{ mkDerivation, base, directory, fetchgit, hpack, hspec
, hspec-discover, lib, QuickCheck, random, time, vector
}:
mkDerivation {
  pname = "swiss-ephemeris";
  version = "1.4.0.0";
  src = fetchgit {
    url = "https://github.com/lfborjas/swiss-ephemeris";
    sha256 = "16kyx48mbq9ywmgvcvajajf6kv2x7g9r9srl3wrjn8p8czvnr616";
    rev = "338bbb5ba39614d93b1a8f752bbb3933bd57cec1";
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
