{ trivialBuild
, fetchFromGitHub
, ...
}:

trivialBuild {
  pname = "related-files";
  version = "main-2024-07-15";

  src = fetchFromGitHub {
    owner = "sstoltze";
    repo = "related-files";
    rev = "ac53bb55d8f7d5e02e731febaf13911fb13ec6ad";
    sha256 = "sha256-tw+t8EU95bzsTpS3Let6IUKoUK0xEGogVySacP08IuQ=";
  };

}
