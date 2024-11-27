{
  trivialBuild,
  fetchFromGitHub,
  ...
}:

trivialBuild {
  pname = "related-files";
  version = "main-2024-11-29";

  src = fetchFromGitHub {
    owner = "sstoltze";
    repo = "related-files";
    rev = "2a904d6c8b3701e900c0a12f493ffd1a92906392";
    sha256 = "sha256-xTkRXEz+AHJ4UWo6LrQv/eQFtcSfQFiLHe6DazVMYHU=";
  };

}
