{ compiler ? "ghc864", pkgs ? import ./nix/packages.nix {} }:

with pkgs;

let
  haskellPackages = haskell.packages.${compiler};
  source = nix-gitignore.gitignoreSource [] ./.;
  drv = haskellPackages.callCabal2nix "codecation" source {};
in
  {
    codecation = drv;
    codecation-shell = haskellPackages.shellFor {
      packages = p: [ drv ];
      buildInputs = with haskellPackages; [
        cabal-install
        hpack
        hlint
        ghcid
      ];
      withHoogle = true;
    };
  }
