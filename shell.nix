{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bifunctors, stdenv }:
      mkDerivation {
        pname = "bimap-quadtree";
        version = "0.0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base bifunctors ];
        description = "biderectional maps";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
