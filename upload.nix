{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
with pkgs.haskell.lib;
{
   upload = sdistTarball (buildStrictly (pkgs.haskell.packages.${compiler}.callPackage ./s3-signer.nix {}));
}
