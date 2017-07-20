{pkgs, fetchFromGitHub}:
let
  python = import ./requirements.nix { inherit pkgs; };
in python.mkDerivation rec {
  name = "${pname}-${version}";
  version = "0.2.2";
  pname = "python-language-server";
  src = fetchFromGitHub {
    owner = "palantir";
    repo = pname;
    rev = version;
    sha256 = "1z8psnyzpfcfgz5ysnd50m14qr78kinl4vl6y54xv1ljgspz4xvs";
  };
  buildInputs = with python.packages; [
    configparser
    future
    jedi
    json-rpc
    pycodestyle
    pyflakes
    yapf
    pluggy
    pytest
  ];
  propagatedBuildInputs = [
  ];
}
