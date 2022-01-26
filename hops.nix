{ mkDerivation, aeson, ansi-terminal, attoparsec, base, bytestring
, conduit, conduit-extra, containers, deepseq, directory, filepath
, http-conduit, http-types, memoize, optparse-applicative, parallel
, process, QuickCheck, resourcet, stdenv, text, transformers
, vector, lib
}:
mkDerivation {
  pname = "hops";
  version = "0.8.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring
    containers deepseq directory filepath
    memoize optparse-applicative text transformers
    vector
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring
    containers deepseq directory filepath
    memoize optparse-applicative parallel text
    transformers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers deepseq directory
    filepath memoize process QuickCheck text transformers vector
  ];
  homepage = "http://akc.is/hops";
  description = "Handy Operations on Power Series";
  license = lib.licenses.bsd3;
}
