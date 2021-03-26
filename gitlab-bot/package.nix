{ buildPythonApplication, isPy3k, python-gitlab, falcon, mypy }:

buildPythonApplication rec {
  pname = "ghc-perf-import-service";
  version = "0.1";
  src = ./.;
  propagatedBuildInputs = [ python-gitlab falcon ];
  disabled = !isPy3k;
  checkInputs = [ mypy ];
  checkPhase = ''
    #mypy --ignore-missing-imports .
  '';
}
