{ compiler ? "ghc864", pkgs ? import ./nix/packages.nix {} }:

with pkgs;

let
  dontCheck = pkgs.haskell.lib.dontCheck;
  llvm-hs = import ./nix/llvm-hs.nix;
  llvm-hs-pretty = import ./nix/llvm-hs-pretty.nix;
  haskellPackages = haskell.packages.${compiler};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      llvm-hs = dontCheck (self.callCabal2nix "llvm-hs" "${llvm-hs}/llvm-hs" { llvm-config = pkgs.llvm_7; });
      llvm-hs-pure = self.callCabal2nix "llvm-hs-pure" "${llvm-hs}/llvm-hs-pure" {};
      llvm-hs-pretty = self.callCabal2nix "llvm-hs-pretty" llvm-hs-pretty {};
    };
  };
  source = nix-gitignore.gitignoreSource [] ./.;
  drv = haskellPkgs.callCabal2nix "codecation" source {};
in
  {
    codecation = drv;
    codecation-shell = haskellPkgs.shellFor {
      packages = p: [ drv ];
      buildInputs = with haskellPkgs; [
        cabal-install
        hpack
        hlint
        ghcid
      ];
      withHoogle = true;
    };
  }
