{ mkDerivation, base, base64-bytestring, blaze-builder, byteable
, bytestring, case-insensitive, cryptohash, http-types, stdenv
, time, utf8-string
}:
mkDerivation {
  pname = "s3-signer";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring blaze-builder byteable bytestring
    case-insensitive cryptohash http-types time utf8-string
  ];
  testHaskellDepends = [ base blaze-builder bytestring time ];
  homepage = "https://github.com/dmjio/s3-signer";
  description = "Pre-signed Amazon S3 URLs";
  license = stdenv.lib.licenses.bsd3;
}
