{ nixpkgs ? import ./nixpkgs.nix, compiler ? "" }:

with nixpkgs;
let ghc = if compiler == "" then haskellPackages else haskell.packages.${compiler};
    in

haskell.lib.shellAware (ghc.callCabal2nix "chronos-bench" (lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"]) {})
