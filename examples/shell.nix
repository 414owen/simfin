{ nixpkgs ? import <unstable> {}, compiler ? "ghc922", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  simfin = import ../default.nix;
  f = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
    simfin = haskellPackages.callPackage simfin;
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
