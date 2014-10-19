{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./hw3.nix {}
