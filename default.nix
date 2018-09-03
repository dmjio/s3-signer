{ compiler ? "ghc843"
, pkgs ? import <nixpkgs> {}
}:
let
  overrides = self: super: {
     s3-signer = self.callPackage ./s3-signer.nix {};
  };
  hPkgs = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
in
  hPkgs.s3-signer
