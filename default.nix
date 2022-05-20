{ mkDerivation, aeson, base, bytestring, composition-extra
, exceptions, http-client, http-client-tls, http-types, lib, tasty
, tasty-hunit, text, time, utf8-string
, Chart, Chart-diagrams, containers, multi-containers
, hlint
}:
mkDerivation {
  pname = "simfin";
  version = "0.1.0";
  src = ./.;
  buildTools = [
    hlint
  ];
  libraryHaskellDepends = [
    aeson base bytestring composition-extra exceptions http-client
    http-client-tls http-types text time utf8-string
  ];
  executableHaskellDepends = [
    base
    Chart
    Chart-diagrams
    time
    containers
    multi-containers
  ];
  testHaskellDepends = [ aeson base tasty tasty-hunit text ];
  license = lib.licenses.mit;
}
