{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps : with ps; [
    criterion
    split
    regex-tdfa
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
  ];
in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = nixPackages;
  }
