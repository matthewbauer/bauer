{stdenv, fetchurl}:
stdenv.mkDerivation rec {
  version = "0.2.0-201707061822";
  pname = "jdt-language-server";
  name = "${pname}-${version}";
  src = fetchurl {
    url = "http://download.eclipse.org/jdtls/snapshots/${name}.tar.gz";
    sha256 = "05q65658ysrw4xcn5fhlp5xfqp70kwkzngdkhhw7vpxx18hb4dfd";
  };
  sourceRoot = ".";
  installPhase = ''
    mkdir $out
    cp -r config_* features plugins $out
  '';
}
