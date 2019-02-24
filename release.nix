{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs.haskell.lib;
with nixpkgs.lib;
let tested = [ "ghc844"
               "ghc861"
               "ghc862"
               "ghc863"
               ];
    eval = x: import ./default.nix { nixpkgs = nixpkgs; compiler = x; };
in
{ sdist = sdistTarball (eval (last tested));
} // genAttrs tested eval
