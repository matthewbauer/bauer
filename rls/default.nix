{rustPlatform, openssl, cmake, fetchFromGitHub}:

with rustPlatform;
buildRustPackage {
  name = "rls";
  src = fetchFromGitHub {
    owner = "rust-lang-nursery";
    repo = "rls";
    rev = "931aba8d0ed2f84972acaada56b998babfb021a0";
    sha256 = "1s5pffp45a6acnxhkb9k4pxb584nfbnd7qhaqckxng21i5bj9s9r";
  };
  buildInputs = [ openssl ];
  nativeBuildInputs = [ cmake ];
  depsSha256 = "0r4im0dcmil6zazbbc85xpxjv0fh0m6kpr1scjqyhqw11wdpv59c";
}
