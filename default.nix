{ mkDerivation, aeson, base, bytestring, composition-extra
, exceptions, http-client, http-client-tls, http-types, lib, tasty
, tasty-hunit, text, time, utf8-string
}:
mkDerivation {
  pname = "simfin";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring composition-extra exceptions http-client
    http-client-tls http-types text time utf8-string
  ];
  testHaskellDepends = [ aeson base tasty tasty-hunit text ];
  license = lib.licenses.mit;
}
