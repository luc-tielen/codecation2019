{ compiler ? "ghc864", pkgs ? import ./nix/packages.nix {} }:

(import ./. { inherit pkgs compiler; }).codecation-shell
