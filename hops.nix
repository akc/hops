{ mkDerivation, aeson, ansi-terminal, attoparsec, base, bytestring
, conduit, conduit-extra, containers, deepseq, directory, filepath
, http-conduit, http-types, memoize, optparse-applicative, parallel
, process, QuickCheck, resourcet, stdenv, text, transformers
, vector
}:
mkDerivation {
  pname = "hops";
  version = "0.8.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal attoparsec base bytestring conduit
    conduit-extra containers deepseq directory filepath http-conduit
    http-types memoize optparse-applicative resourcet text transformers
    vector
  ];
  executableHaskellDepends = [
    aeson ansi-terminal attoparsec base bytestring conduit
    conduit-extra containers deepseq directory filepath http-conduit
    http-types memoize optparse-applicative parallel resourcet text
    transformers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers deepseq directory
    filepath memoize process QuickCheck text transformers vector
  ];
  homepage = "http://akc.is/hops";
  description = "Handy Operations on Power Series";
  license = stdenv.lib.licenses.bsd3;
}
