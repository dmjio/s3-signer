{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
with pkgs.haskell.lib;
{
   upload = sdistTarball (buildStrictly (pkgs.haskell.packages.${compiler}.callPackage ./s3-signer.nix {}));
}
