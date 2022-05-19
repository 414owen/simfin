{ mkDerivation, base, Chart, Chart-diagrams, lib, simfin, time }:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base Chart Chart-diagrams simfin time ];
  license = lib.licenses.mit;
}
