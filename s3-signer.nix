{ mkDerivation, base, base64-bytestring, cryptohash, http-types
, stdenv, time, utf8-string
}:
mkDerivation {
  pname = "s3-signer";
  version = "0.3.0.0";
  src = ./.;
  buildDepends = [
    base base64-bytestring cryptohash http-types time utf8-string
  ];
  homepage = "https://github.com/dmjio/s3-signer";
  description = "Pre-signed Amazon S3 URLs";
  license = stdenv.lib.licenses.bsd3;
}
