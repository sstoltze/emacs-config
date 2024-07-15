{ trivialBuild
, fetchFromGitHub
, ...
}:

trivialBuild {
  pname = "document-sections";
  version = "main-2024-07-15";

  src = fetchFromGitHub {
    owner = "sstoltze";
    repo = "document-sections";
    rev = "3252c4fb4f1e03d36bf197903f9050217d4a0274";
    sha256 = "sha256-rtfTL2QPmb3+nfsY8gkUpNqez5t06u7FhZvqZOBJ274=";
  };

}
