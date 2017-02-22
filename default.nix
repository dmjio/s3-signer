{ compiler ? "ghc802" }:
  let
   config = {
     packageOverrides = pkgs: with pkgs.haskell.lib; {
       haskell.packages.${compiler} = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: rec {
            s3-signer = buildStrictly (self.callPackage ./s3-signer.nix { });
          };
        };
      };
   };
   in
     let
       pkgs = import <nixpkgs> { inherit config; };
       s3-signer = pkgs.haskell.packages.${compiler}.s3-signer;
     in { inherit s3-signer; }
