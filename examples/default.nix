{ mkDerivation, base, Chart, Chart-diagrams, lib, simfin, time
, containers, multi-containers, hlint }:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  devTools = [
    hlint
  ];
  executableHaskellDepends = [
    base
    Chart
    Chart-diagrams
    simfin
    time
    containers
    multi-containers
  ];
  license = lib.licenses.mit;
}
