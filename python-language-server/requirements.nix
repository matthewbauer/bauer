# generated using pypi2nix tool (version: 1.8.0)
# See more at: https://github.com/garbas/pypi2nix
#
# COMMAND:
#   pypi2nix -V 3.5 -r requirements.txt
#

{ pkgs ? import <nixpkgs> {}
}:

let

  inherit (pkgs) makeWrapper;
  inherit (pkgs.stdenv.lib) fix' extends inNixShell;

  pythonPackages =
  import "${toString pkgs.path}/pkgs/top-level/python-packages.nix" {
    inherit pkgs;
    inherit (pkgs) stdenv;
    python = pkgs.python35;
  };

  commonBuildInputs = [];
  commonDoCheck = false;

  withPackages = pkgs':
    let
      pkgs = builtins.removeAttrs pkgs' ["__unfix__"];
      interpreter = pythonPackages.buildPythonPackage {
        name = "python35-interpreter";
        buildInputs = [ makeWrapper ] ++ (builtins.attrValues pkgs);
        buildCommand = ''
          mkdir -p $out/bin
          ln -s ${pythonPackages.python.interpreter}               $out/bin/${pythonPackages.python.executable}
          for dep in ${builtins.concatStringsSep " "               (builtins.attrValues pkgs)}; do
            if [ -d "$dep/bin" ]; then
              for prog in "$dep/bin/"*; do
                if [ -f $prog ]; then
                  ln -s $prog $out/bin/`basename $prog`
                fi
              done
            fi
          done
          for prog in "$out/bin/"*; do
            wrapProgram "$prog" --prefix PYTHONPATH : "$PYTHONPATH"
          done
          pushd $out/bin
          ln -s ${pythonPackages.python.executable} python
          popd
        '';
        passthru.interpreter = pythonPackages.python;
      };
    in {
      __old = pythonPackages;
      inherit interpreter;
      mkDerivation = pythonPackages.buildPythonPackage;
      packages = pkgs;
      overrideDerivation = drv: f:
        pythonPackages.buildPythonPackage (drv.drvAttrs // f drv.drvAttrs);
      withPackages = pkgs'':
        withPackages (pkgs // pkgs'');
    };

  python = withPackages {};

  generated = self: {

    "configparser" = python.mkDerivation {
      name = "configparser-3.5.0";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/7c/69/c2ce7e91c89dc073eb1aa74c0621c3eefbffe8216b3f9af9d3885265c01c/configparser-3.5.0.tar.gz"; sha256 = "5308b47021bc2340965c371f0f058cc6971a04502638d4244225c49d80db273a"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "This library brings the updated configparser from Python 3.5 to Python 2.6-3.5.";
      };
    };



    "future" = python.mkDerivation {
      name = "future-0.16.0";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/00/2b/8d082ddfed935f3608cc61140df6dcbf0edea1bc3ab52fb6c29ae3e81e85/future-0.16.0.tar.gz"; sha256 = "e39ced1ab767b5936646cedba8bcce582398233d6a627067d4c6a454c90cfedb"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "Clean single-source support for Python 3 and 2";
      };
    };



    "jedi" = python.mkDerivation {
      name = "jedi-0.10.2";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/80/b9/4e9b0b999deeec8a91cb84e567380853a842e6c387c9d39b8fc9a49953fa/jedi-0.10.2.tar.gz"; sha256 = "7abb618cac6470ebbd142e59c23daec5e6e063bfcecc8a43a037d2ab57276f4e"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "An autocompletion tool for Python that can be used for text editors.";
      };
    };



    "json-rpc" = python.mkDerivation {
      name = "json-rpc-1.10.3";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/df/cc/1ba8ca75b9e45eae5d146b9e45f64d344f616f89422ecfab9a7a6f0968b8/json-rpc-1.10.3.tar.gz"; sha256 = "d13e24396ac975ed0ae2f396833dc64148503939b9dff0a7a5ac16918f392585"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "JSON-RPC transport realisation";
      };
    };



    "pluggy" = python.mkDerivation {
      name = "pluggy-0.4.0";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/38/e2/b23434f4030bbb1af3bcdbb2ecff6b11cf2e467622446ce66a08e99f2ea9/pluggy-0.4.0.zip"; sha256 = "dd841b5d290b252cf645f75f3bd37ceecfa0f36394ab313e4f785fe68a4081a4"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "plugin and hook calling mechanisms for python";
      };
    };



    "py" = python.mkDerivation {
      name = "py-1.4.34";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/68/35/58572278f1c097b403879c1e9369069633d1cbad5239b9057944bb764782/py-1.4.34.tar.gz"; sha256 = "0f2d585d22050e90c7d293b6451c83db097df77871974d90efd5a30dc12fcde3"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "library with cross-python path, ini-parsing, io, code, log facilities";
      };
    };



    "pycodestyle" = python.mkDerivation {
      name = "pycodestyle-2.3.1";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/e1/88/0e2cbf412bd849ea6f1af1f97882add46a374f4ba1d2aea39353609150ad/pycodestyle-2.3.1.tar.gz"; sha256 = "682256a5b318149ca0d2a9185d365d8864a768a28db66a84a2ea946bcc426766"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "Python style guide checker";
      };
    };



    "pyflakes" = python.mkDerivation {
      name = "pyflakes-1.5.0";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/5b/b7/dcd6ebc826065ca4ccd2406aac4378e1df6eb91124625d45d520219932a1/pyflakes-1.5.0.tar.gz"; sha256 = "aa0d4dff45c0cc2214ba158d29280f8fa1129f3e87858ef825930845146337f4"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "passive checker of Python programs";
      };
    };



    "pytest" = python.mkDerivation {
      name = "pytest-3.1.3";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/15/c8/6b42bf58f91d72416806472512bb67dabc6edb5a6a8ace29853ff940400a/pytest-3.1.3.tar.gz"; sha256 = "095e1832f7e424563c95daf4d8d3c865052b80e139cdd2f9610a986ee8526206"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."py"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "pytest: simple powerful testing with Python";
      };
    };



    "yapf" = python.mkDerivation {
      name = "yapf-0.16.3";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/b2/da/4f6fd80c0807b1f17398289f5e1535cc62e80ad06cfc7635227c8788b4a7/yapf-0.16.3.tar.gz"; sha256 = "429d3e0eb0111dbb90ee5d4b863f872e65ebb78ddf9529019754e9627c20b8e3"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.asl20;
        description = "A formatter for Python code.";
      };
    };

  };
  overrides = import ./requirements_override.nix { inherit pkgs python; };
  commonOverrides = [

  ];

in python.withPackages
   (fix' (pkgs.lib.fold
            extends
            generated
            ([overrides] ++ commonOverrides)
         )
   )